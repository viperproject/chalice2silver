package ch.ethz.inf.pm.semper.chalice2sil.translation

/**
  * Author: Christian Klauser
  */

import ch.ethz.inf.pm.semper.chalice2sil.{ChaliceSuite}
import org.scalatest.matchers.{MatchResult, BeMatcher, ShouldMatchers}
import silAST.types.{referenceType, integerType}
import silAST.expressions.PermissionExpression
import silAST.programs.symbols.ProgramVariable
import silAST.methods.implementations.FieldAssignmentStatement
import silAST.expressions.terms.{FullPermissionTerm, ProgramVariableTerm, FieldReadTerm, LiteralTerm}

class Basic extends ChaliceSuite with ShouldMatchers {

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
  
  'locals.chalice {
    program.methods should have size (2)

    val main = getMainImpl

    val locals = main.locals
    
    locals should have size (2)
    locals foreach (_.dataType should be (integerType))
    
    val other = getImpl("Main::other")
    val otherLocals = other.locals
    
    otherLocals should have size (1) //only variables that are actually used will be added in SIL
  }  
  
  'directFieldUpdate.chalice {
    val main = getMainImpl
    
    main.locals should have size (1)
    main.locals.head.name.startsWith("m#") should be (true)
  }

  'singleFieldUpdate.chalice {
    val main = getMainImpl
    
    main.body.startNode.statements foreach  (s => {
      s should be (instanceOf[FieldAssignmentStatement])

      val fa = s.asInstanceOf[FieldAssignmentStatement]

      fa.field.name should be ("Main::f")
      fa.field.dataType should be (integerType)
      fa.source should be (instanceOf[LiteralTerm])
      fa.target.name should be ("this")
      fa.target.dataType should be (referenceType)
    })
  }

}
