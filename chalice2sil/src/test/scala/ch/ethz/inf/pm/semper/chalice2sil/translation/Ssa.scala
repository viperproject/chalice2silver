package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil.ChaliceSuite
import silAST.programs.symbols.ProgramVariable
import org.scalatest.matchers.{MatchResult, BeMatcher, ShouldMatchers}

class Ssa extends ChaliceSuite with ShouldMatchers {

  // COMMON

  def getImpl(methodName : String) = {
    val mainOpt = program.methods.find(_.name == methodName)

    mainOpt should be ('defined)
    mainOpt.get.implementations should have size (1)

    mainOpt.get.implementations.head
  }

  def getMainImpl = getImpl("Main::main")

  def instanceOf[T](implicit valueClass : Manifest[T]) : BeMatcher[AnyRef] = { //
    BeMatcher[AnyRef](
      (obj : AnyRef)  => {
        val errorBegin = "he object is expected to have type %s".format(valueClass)
        val errorBeginNeg = "he object is expected not to have type %s".format(valueClass)
        if(obj == null){
          val nullEnd = " but is null"
          MatchResult(obj != null, "T" + errorBegin + nullEnd, "T" + errorBeginNeg + nullEnd,
            "t" + errorBegin + nullEnd, "t" + errorBeginNeg + nullEnd)
        }
        val errorEnd = " but has actually type %s".format(obj.getClass)
        MatchResult(valueClass.erasure.isInstance(obj),"T" + errorBegin + errorEnd, "T" + errorBeginNeg + errorEnd, "t" + errorBegin + errorEnd, "t" + errorBeginNeg + errorEnd)
      })
  }

  def matchCast[T](value : AnyRef)(implicit valueClass : Manifest[T]) : T = {
    value should be (instanceOf[T](valueClass))
    value.asInstanceOf[T]
  }

  def matchContains(variables : Seq[ProgramVariable], name : String) : ProgramVariable = {
    val vOpt = variables.find(_.name == name)
    assert(vOpt.isDefined,"The variable sequence is expected to contain a variable called %s".format(name))
    vOpt.get
  }
  
  // CHALICE FILES
  
  'simpleLoop.chalice {
    val main = getMainImpl
    main.body should not be (null)
  }
  
}
