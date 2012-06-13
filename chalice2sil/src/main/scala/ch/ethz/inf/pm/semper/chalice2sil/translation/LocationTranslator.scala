package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.ExpressionFactory
import silAST.expressions.terms.PTerm
import silAST.expressions.util.PTermSequence

trait LocationTranslator extends ProgramEnvironment {

  /**
    * An id that is unique within the program and is used to identify this location
    * in expressions.
    */
  def id : Int

  def locationLiteral(expressionFactory : ExpressionFactory, reference : PTerm) : PTerm = {
    val fieldLiteral = expressionFactory.makeIntegerLiteralTerm(id,reference.sourceLocation)
    expressionFactory.makePDomainFunctionApplicationTerm(
      prelude.Pair.Location.create,PTermSequence(reference,fieldLiteral),reference.sourceLocation)
  }

}
