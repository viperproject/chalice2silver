package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil._
import silAST.source.SourceLocation
import silAST.symbols.logical.And
import silAST.expressions.TrueExpression

/**
  * Created with IntelliJ IDEA.
  * User: Christian
  * Date: 18/06/12
  * Time: 16:20
  * To change this template use File | Settings | File Templates.
  */

class MonitorInvariantPredicateTranslator(environment : ProgramEnvironment, chaliceClass : chalice.Class, id : Int)
  extends PredicateTranslator(environment,id) {

  val sourceLocation = chaliceClass.MonitorInvariants.headOption.map(astNodeToSourceLocation(_)).getOrElse(astNodeToSourceLocation(chaliceClass))

  def createPredicateFactory() = programFactory.getPredicateFactory(fullMonitorInvariantName(chaliceClass),
    sourceLocation,
    List("Predicate capturing the monitor invariant of Chalice class " + chaliceClass.id + "."))

  /**
    * Translates the body of the predicate. Must be called exactly once.
    */
  def translate() {
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
}
