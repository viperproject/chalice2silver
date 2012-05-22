package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import silAST.source.SourceLocation
import silAST.expressions.util.TermSequence
import silAST.expressions.terms.IntegerLiteralTerm
import util.DerivedFactoryCache
import silAST.programs.symbols.{ProgramVariable, PredicateFactory}
import chalice.Variable

/**
  * @author Christian Klauser
  */
class PredicateTranslator(environment : ProgramEnvironment, val predicate : chalice.Predicate, val id : Int)
  extends DerivedProgramEnvironment(environment)
  with MemberEnvironment
{
  val predicateFactory : PredicateFactory = programFactory.getPredicateFactory(fullPredicateName(predicate))(predicate)

  def programVariables = new DerivedFactoryCache[chalice.Variable,String,ProgramVariable] {
    /**
      * Derives the key from a supplied prototype.
      *
      * Note that, while prototypes may be mutable, the key derived from it should be the same while the prototype is in
      * the same state, i.e., you can't just return an incrementing integer. Otherwise the following code would not work:
      * {{{
      *   val cache = new DerivedFactoryCache[P,K,V] {...}
      *   val prototype = ...
      *   assert(cache(prototype) == cache(prototype)) //deriveKey must return the same key for both lookups
      * }}}

    }* @param p The prototype to derive a key from.
      * @return the key for the prototype
      */
    protected def deriveKey(p : Variable) = p.UniqueName


    /**
      * Determines the appropriate key for a value.
      *
      * By default, this method throws an `UnsupportedOperationException`.
      * If you want the functionality of the `AdjustableCache` trait, override this method and provide an implementation.
      * @param value The value for which the key should be derived.
      * @return The (unique) key for this value.
      */
    override protected def deriveKeyFromValue(value : ProgramVariable) = value.name

    /**
      * In case of a cache-miss, this method is called to create the missing entry. The key with which the value will
      * be added to the cache has been determined before via the `deriveKey` method.
      * @param p  The prototype to create the value from.
      * @return The value to be added to the cache.
      */
    protected def construct(p : Variable) = throw new UnsupportedOperationException("Predicates cannot define new program variables.")
  }

  def thisVariable = predicateFactory.thisVar

  def environmentReadFractionTerm(sourceLocation : SourceLocation) = {
    val idLiteral = currentExpressionFactory.makeIntegerLiteralTerm(id)(predicate)
    val reference = currentExpressionFactory.makeProgramVariableTerm(thisVariable)(predicate)
    currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Predicate.readFraction,
      TermSequence(idLiteral,reference))(predicate)
  }

  def currentExpressionFactory = predicateFactory

  def nameSequence = util.NameSequence()

  /**
    * Translates the body of the predicate. Must be called exactly once.
    */
  def translate() {
    programVariables.addExternal(thisVariable)
    val translator = new DefaultCodeTranslator(this) {
      override protected def readFraction(location : SourceLocation) = environmentReadFractionTerm(location)
    }
    predicateFactory.setExpression(translator.translateExpression(predicate.definition))
  }

}