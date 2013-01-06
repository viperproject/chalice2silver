package semper.chalice2sil.translation

import semper.sil.ast.expressions.ExpressionFactory
import semper.sil.ast.expressions.terms.Term
import semper.sil.ast.expressions.util.TermSequence

trait LocationTranslator extends ProgramEnvironment {

  /**
    * An id that is unique within the program and is used to identify this location
    * in expressions.
    */
  def id : Int

  def locationLiteral(expressionFactory : ExpressionFactory, reference : Term) : Term = {
    val fieldLiteral = expressionFactory.makeIntegerLiteralTerm(id,reference.sourceLocation)
    expressionFactory.makePDomainFunctionApplicationTerm(
      prelude.Pair.Location.create,TermSequence(reference,fieldLiteral),reference.sourceLocation)
  }

}
