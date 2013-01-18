package semper.chalice2sil.translation

import semper.chalice2sil
import chalice2sil._
import semper.sil.ast.source.SourceLocation
import semper.sil.ast.expressions.util.ExpressionSequence
import util.DerivedFactoryCache
import semper.sil.ast.programs.symbols.{ProgramVariable, PredicateFactory}
import chalice.Variable
import semper.sil.ast.types.nullFunction

/**
  * @author Christian Klauser
  */
abstract class PredicateTranslator(environment : ProgramEnvironment,
  /**
  * An id that is unique within the program and is used to identify this predicate
  * in expressions.
  */
  val id : Int)
  extends DerivedProgramEnvironment(environment)
  with MemberEnvironment
  with LocationTranslator
{
  def sourceLocation : SourceLocation
  def createPredicateFactory() : PredicateFactory

  val predicateFactory : PredicateFactory = createPredicateFactory()

  val programVariables = new DerivedFactoryCache[chalice.Variable,String,ProgramVariable] {
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
      * @param p The prototype to derive a key from.
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

  def environmentReadFractionExpression(sourceLocation : SourceLocation) = {
    val idLiteral = currentExpressionFactory.makeIntegerLiteralExpression(id,sourceLocation)
    val reference = currentExpressionFactory.makeProgramVariableExpression(thisVariable,sourceLocation)
    currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Predicate().readFraction,
      ExpressionSequence(idLiteral,reference),sourceLocation)
  }

  def environmentCurrentThreadExpression(sourceLocation : SourceLocation) = {
    report(messages.LockingRelatedInPredicate(sourceLocation))
    currentExpressionFactory.makeDomainFunctionApplicationExpression(nullFunction,ExpressionSequence(),sourceLocation,List(
      "$CurrentThread not available in predicates/invariants. A corresponding error message has been emitted."))
  }

  def currentExpressionFactory = predicateFactory

  def nameSequence = util.NameSequence()

}
