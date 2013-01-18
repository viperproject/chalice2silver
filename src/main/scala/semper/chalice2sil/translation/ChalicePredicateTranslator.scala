package semper.chalice2sil.translation

import semper.chalice2sil._
import semper.sil.ast.source.SourceLocation

class ChalicePredicateTranslator(environment : ProgramEnvironment, val predicate : chalice.Predicate, id : Int)
  extends PredicateTranslator(environment,id) {
  /**
    * Translates the body of the predicate. Must be called exactly once.
    */
  def translate() {
    programVariables.addExternal(thisVariable)
    val translator = new DefaultCodeTranslator(this) {
      override protected def readFraction(location : SourceLocation) = environmentReadFractionExpression(location)
    }
    predicateFactory.setExpression(translator.translateExpression(predicate.definition))
  }

  def createPredicateFactory() = programFactory.getPredicateFactory(fullPredicateName(predicate),sourceLocation)

  override val sourceLocation : SourceLocation = predicate
}
