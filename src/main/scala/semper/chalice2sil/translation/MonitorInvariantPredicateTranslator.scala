package semper.chalice2sil.translation

import semper.chalice2sil._
import semper.sil.ast.source.SourceLocation
import semper.sil.ast.symbols.logical.And
import semper.sil.ast.expressions.TrueExpression

class MonitorInvariantPredicateTranslator(environment : ProgramEnvironment, chaliceClass : chalice.Class, id : Int)
  extends PredicateTranslator(environment,id) {

  val sourceLocation = chaliceClass.MonitorInvariants.headOption.map(astNodeToSourceLocation(_)).getOrElse(astNodeToSourceLocation(chaliceClass))

  def createPredicateFactory() = programFactory.getPredicateFactory(fullMonitorInvariantName(chaliceClass),
    sourceLocation,
    List("Predicate capturing the monitor invariant of Chalice class " + chaliceClass.id + "."))

  /**
    * Translates the body of the predicate. Must be called exactly once.
    */
  private[this] def translate() {
    programVariables.addExternal(thisVariable)
    val translator = new DefaultCodeTranslator(this) {
      override protected def readFraction(location : SourceLocation) = environmentReadFractionTerm(location)
    }
    val expr = chaliceClass.MonitorInvariants
      // translate individual monitor invariant expressions
      .map(mi => translator.translateExpression(mi.e))
      // connect them with `&&`
      .reduceOption((left,right) => currentExpressionFactory.makeBinaryExpression(
        And()(left.sourceLocation),left,right,
        right.sourceLocation,List("Conjunction of monitor invariants into one expression.")))
      // handle the case where we don't have a single monitor invariant
      .getOrElse(TrueExpression()(sourceLocation,List("Class " + chaliceClass.id + " does not have a monitor invariant.")))
    predicateFactory.setExpression(expr)
  }

  translate()
}
