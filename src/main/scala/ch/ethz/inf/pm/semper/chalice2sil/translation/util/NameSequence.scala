package ch.ethz.inf.pm.semper.chalice2sil.translation.util

/**
  * Author: Christian Klauser
  */

abstract class NameSequence {
  val alphabet : IndexedSeq[Char]

  private def N = alphabet.length

  protected def intPower(base : Int, exp : Int) : Int = {
    require(exp >= 0)
    var exponent = exp
    var powerBase = base
    var result = 1;
    while(exponent > 0) {
      if((exponent & 1) != 0)
        result *= powerBase
      exponent >>= 1
      powerBase *= powerBase
    }
    result
  }

  protected def lex(len : Int, index : Int) : List[Char] =
    if(len <= 0)
      Nil
    else
      alphabet(index % N) :: lex(len - 1, index / N)

  protected def genWord(len : Int, index : Int) = {
    val sb = new StringBuilder(len)
    sb ++= lex(len, index)
    sb.reverseContents()
    sb.toString()
  }

  def allNames : Stream[String] =
    Stream.from(1)
      .flatMap(length => Stream.range(0, intPower(N, length)).map(genWord(length, _)))

  protected var currentName : Option[Stream[String]] = None

  def nextName = {
    val cn = currentName match {
      case None => allNames
      case Some(s) => s
    }
    val name = cn.head
    currentName = Some(cn.tail)
    name
  }

}

object NameSequence {

  class LowerAlphabet extends NameSequence {
    override val alphabet = ('a' to 'z')
  }

  def apply() = new LowerAlphabet
}