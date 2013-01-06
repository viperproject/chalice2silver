package semper.chalice2sil.translation

import semper.chalice2sil
import chalice2sil._
import semper.sil.ast.source.SourceLocation
import semper.sil.ast.expressions.util.TermSequence
import util.DerivedFactoryCache
import semper.sil.ast.programs.symbols.{FunctionFactory, ProgramVariable}
import semper.sil.ast.types.nullFunction
import semper.sil.ast.expressions.terms.Term
import semper.sil.ast.expressions.Expression

/**
  * @author Christian Klauser
  */
class FunctionTranslator(environment : ProgramEnvironment, val function : chalice.Function)
  extends DerivedProgramEnvironment(environment)
  with MemberEnvironment
  with TypeTranslator {

  val functionFactory : FunctionFactory = programFactory.getFunctionFactory(
    fullFunctionName(function),
    function.ins.map(
      v => (v:SourceLocation,v.UniqueName, translateTypeExpr(v.t))),
    translateTypeExpr(function.out),function)

  val programVariables = {
    val cache = new DerivedFactoryCache[chalice.Variable, String, ProgramVariable] {
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
      protected def deriveKey(p : chalice.Variable) = p.UniqueName


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
      protected def construct(p : chalice.Variable) = throw new
          UnsupportedOperationException("Functions cannot define new program variables. Missing variable mapping for " + p.id + " : " + p.t.typ)
    }
    functionFactory.parameters.foreach(cache.addExternal)
    cache
  }

  def thisVariable = functionFactory.thisVar
  def resultVariable = functionFactory.resultVar

  def environmentReadFractionTerm(sourceLocation : SourceLocation) = {
    val reference = currentExpressionFactory.makeProgramVariableTerm(thisVariable,sourceLocation)
    currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Function().readFraction,
      TermSequence(reference),sourceLocation)
  }

  def environmentCurrentThreadTerm(sourceLocation : SourceLocation) = {
    report(messages.LockingRelatedInPredicate(sourceLocation))
    currentExpressionFactory.makeDomainFunctionApplicationTerm(nullFunction,TermSequence(),sourceLocation,List(
      "$CurrentThread not available in functions. A corresponding error message has been emitted."))
  }

  def currentExpressionFactory = functionFactory

  def nameSequence = util.NameSequence()

  /**
    * Translates the body of the predicate. Must be called exactly once.
    */
  def translate() {
    programVariables.addExternal(thisVariable)
    programVariables.addExternal(functionFactory.resultVar)
    val translator = new DefaultCodeTranslator(this) {
      override protected def readFraction(location : SourceLocation) = environmentReadFractionTerm(location)

      override protected def termTranslation = resultTranslation orElse super.termTranslation

      protected override def translateAccessExpression(permission : chalice.Permission)(body : Term => Expression) : Expression = permission match {
        case chalice.Star
           | chalice.Full
           | chalice.Frac(chalice.IntLiteral(100)) => {
          // Can keep these as-is
          super.translateAccessExpression(permission)(body)
        }
        case chalice.Epsilon
           | chalice.MethodEpsilon
           | chalice.ForkEpsilon(_)
           | chalice.Epsilons(_)
           | chalice.PredicateEpsilon(_) => {
          // Convert to rd*(...)
          super.translateAccessExpression(chalice.Star)(body)
        }
        case x => {
          // Don't know how to convert
          report(messages.PermissionTooComplicatedForPredicateOrFunction(x))
          dummyExpr(currentExpressionFactory,x)
        }
      }

      private val resultTranslation = matchingTerm {
        case r@chalice.Result() => currentExpressionFactory.makeProgramVariableTerm(resultVariable,r)
      }
    }
    function.spec foreach {
      case chalice.Precondition(e) => functionFactory.addPrecondition(translator.translateExpression(e))
      case chalice.Postcondition(e) => functionFactory.addPostcondition(translator.translateExpression(e))
      case otherNode =>
        report(messages.UnknownAstNode(otherNode))
    }
    functionFactory.setBody(translator.translateTerm(function.definition.get))
    functionFactory.setMeasure(currentExpressionFactory.makeIntegerLiteralTerm(1,function))
  }

}
