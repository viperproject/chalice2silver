package semper.chalice2sil.translation

import semper.sil.ast.expressions.ExpressionFactory
import semper.sil.ast.expressions.Expression
import semper.sil.ast.expressions.util.ExpressionSequence

trait LocationTranslator extends ProgramEnvironment {

  /**
    * An id that is unique within the program and is used to identify this location
    * in expressions.
    */
  def id : Int

  def locationLiteral(expressionFactory : ExpressionFactory, reference : Expression) : Expression = {
    val fieldLiteral = expressionFactory.makeIntegerLiteralExpression(id,reference.sourceLocation)
    expressionFactory.makeDomainFunctionApplicationExpression(
      prelude.Pair.Location.create,ExpressionSequence(reference,fieldLiteral),reference.sourceLocation)
  }

}
