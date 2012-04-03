package ch.ethz.inf.pm.semper.chalice2sil.translation.util

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
  require(escapeChar.isValidByte && escapeChar > 0)
  val escapeSequence : String = escapeChar.toString
  val doubleEscapeSequence : String = escapeSequence + escapeSequence

  def mangleInto(text : String, builder : StringBuilder) {
    for(c <- text){
      if (c == escapeChar) {
        builder.append(doubleEscapeSequence)
      } else if(c.toInt > 127) {
        builder.append(escapeChar)
        val esc : String = {
          val num = c.toInt.toHexString
          "".padTo((4-num.length()) max 0,'0') + num
        }
        assert(esc.length() == 4)
        builder.append(esc)
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
    var lastWindow : String = null
    var skip = 0
    for(window <- mangledText.sliding(5)){
      lastWindow = window
      if(skip > 0){
        skip -= 1
      } else {
        if (window.startsWith(doubleEscapeSequence)){
          skip = 1
          builder.append(escapeChar)
        } else if(window.startsWith(escapeSequence) && window.length() == 5) {
          val codePoint = Integer.parseInt(window.substring(1),16)
          builder.append(codePoint.toChar)
          skip = 4
        } else {
          builder.append(window(0))
          skip = 0
        }
      }
    }
    if(lastWindow != null){
      val rest = lastWindow
        .substring(skip+1)
        .replaceAllLiterally(doubleEscapeSequence,escapeSequence)
      builder.append(rest)
    }
    builder.toString()
  }
}
