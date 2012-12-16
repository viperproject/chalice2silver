package semper.chalice2sil.translation.util

import collection._
import semper.chalice2sil.util.Trie
import immutable.{WrappedString, StringLike}

/**
  * Unicode has been around for over 20 years now. One might think that by now we should have solved the problem of
  * encoding text.
  * This class takes arbitrary unicode text and produces a pure ASCII (≤127) representation. It uses the escape char
  * to insert UTF-16 code units encoded as a four digit hexadecimal number.
  * Assuming that `_` is used as the escape character, `fλ_5` is translated to `f_03BB__5`. This scheme has the nice
  * property that valid identifiers in modern programming languages are translated to identifiers that are valid
  * in programming languages that are stuck in ASCII-land.
  * @author Christian Klauser
  */
class UnicodeMangler(val escapeChar : Char) {
  import UnicodeTable._

  require(escapeChar.isValidByte && escapeChar > 0)
  val escapeSequence : String = escapeChar.toString
  val doubleEscapeSequence : String = escapeSequence + escapeSequence

  val windowSize = (forward.values.toIterable.map(_.length).max + 1) max 5
  
  def mangleInto(text : String, builder : StringBuilder) {
    for(c <- text){
      if (c == escapeChar) {
        builder.append(doubleEscapeSequence)
      } else if(c.toInt > 127) {
        builder.append(escapeChar)
        forward.get(c) match {
          case Some(seq) =>
            assert(seq.length + 1 <= windowSize)
            builder.append(seq)
          case None => //fallback to encoding Unicode Code Point
            val esc : String = {
              val num = c.toInt.toHexString
              "".padTo((4-num.length()) max 0,'0') + num
            }
            assert(esc.length() == 4,"Whoa, encountered character that does not fit into a single UTF-16 code unit (i.e. a single Java char).")
            builder.append(esc)
        }
      } else {
        builder.append(c)
      }
    }
  }
  
  def mangle(text : String) : String = {
    val builder = new StringBuilder(text.length())
    mangleInto(text,builder)
    builder.toString()
  }
  
  def unmangle(mangledText : String) : String = {
    val builder = new StringBuilder()
    var skip = 0
    val mangledTextLength = mangledText.length
    for(offset <- 0.to(mangledText.length-1)){
      if(skip > 0){
        skip -= 1
      } else {
        val length = windowSize min (mangledTextLength-offset)
        if (mangledText.startsWith(doubleEscapeSequence,offset)){
          skip = 1
          builder.append(escapeChar)
        } else if(mangledText.startsWith(escapeSequence,offset)){
          backwardTrie.shortestMatch(mangledText.view(offset+1,offset+length)) match {
            case Some(c) =>
              builder.append(c)
              skip = forward(c).length
            case _ if length >= 5 =>
              val codePoint = Integer.parseInt(mangledText.substring(offset+1,4),16)
              builder.append(codePoint.toChar)
              skip = 4
            case _ =>
              throw new MangleException("Cannot decode escape sequence \"%s\".".format(mangledText.substring(offset,length)),mangledText)
          }
        } else {
          builder.append(mangledText(offset))
          skip = 0
        }
      }
    }
    builder.toString()
  }
}
