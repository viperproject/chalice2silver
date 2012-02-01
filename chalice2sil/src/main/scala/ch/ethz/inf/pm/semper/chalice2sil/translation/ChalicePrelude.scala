package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.source.noLocation
import silAST.types.DataTypeSequence._
import silAST.expressions.util.DTermSequence._
import silAST.symbols.logical.Not._
import silAST.types.DataTypeSequence
import silAST.expressions.util.DTermSequence
import silAST.symbols.logical.Not

/**
 * Author: Christian Klauser
 */

class ChalicePrelude(programEnvironment : ProgramEnvironment) {
  
  private val programFactory = programEnvironment.programFactory

  object Boolean {
    private val factory = programFactory.getDomainFactory("Boolean",Nil)(noLocation)
    val Type = programFactory.makeNonReferenceDataType(noLocation,factory,DataTypeSequence())
    val TrueLiteral = factory.defineDomainFunction(noLocation,"BooleanTrue",DataTypeSequence(),Type)
    val FalseLiteral = factory.defineDomainFunction(noLocation,"BooleanFalse",DataTypeSequence(),Type)

    factory.addDomainAxiom(noLocation,"trueAndFalseNotEqual",{
        val eq = factory.makeDEqualityExpression(noLocation,
          factory.makeDDomainFunctionApplicationTerm(noLocation,TrueLiteral,DTermSequence()),
          factory.makeDDomainFunctionApplicationTerm(noLocation,FalseLiteral,DTermSequence()))
        factory.makeDUnaryExpression(noLocation,Not(),eq)
      }
    )

    lazy val Domain = factory.compile()
  }
}