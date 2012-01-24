package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil.translation.NameSequence.LowerAlphabet

/**
 * Author: Christian Klauser
 */

abstract class NameSequence {
  val alphabet : IndexedSeq[Char]
  private val N = alphabet.length

  protected def intPower(base : Int, exp : Int):Int = {
    require(exp >= 0)
    var exponent = exp
    var powerBase = base
    var result = 1;
    while(exponent >= 0){
      if((exponent & 1) != 0)
        result *= powerBase
      exponent >>= 1
      powerBase *= powerBase
    }
    result
  }

  protected def lex(len : Int, index : Int) : List[Char] =  
    if (len <= 0) 
      Nil
    else
      alphabet(index % N) :: lex(len-1,index / N)
  
  protected def genWord(len : Int, index : Int) = {
    val  sb = new StringBuilder(len)
    sb ++= lex (len,index)
    sb.reverseContents()
    sb.toString()
  }

  def allNames : Stream[String] =
    Stream.from(1)
    .flatMap(length => Stream.range(0,intPower(N,length)).map((length,_)))
    .map(t => genWord(t._1,t._2))

  protected var currentName = allNames

  def nextName = {
    val name = currentName.head
    currentName = currentName.tail
    name
  }

}

object NameSequence {
  class LowerAlphabet extends NameSequence{
    override val alphabet = ('a' to 'z')
  }
  def apply() = new LowerAlphabet
}