package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil._

import silAST.expressions.terms.Term
import silAST.source.SourceLocation
import silAST.expressions.util.TermSequence
import silAST.types.{permissionAddition, permissionSubtraction, permissionMultiplication, permissionIntegerMultiplication}

/**
  * @author Christian Klauser
  */
trait PermissionTranslator extends TermTranslator {
  
  def translatePermission(permission : chalice.Permission) : Term = permissionTranslation(permission)

  protected def matchingPermission(partialFunction : PartialFunction[chalice.Permission, Term]) = partialFunction

  protected def permissionTranslation = matchingPermission {
    case p@chalice.PermPlus(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(permissionAddition,
        TermSequence(translatePermission(lhs),translatePermission(rhs)),p)
    case m@chalice.PermMinus(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(permissionSubtraction,
        TermSequence(translatePermission(lhs),translatePermission(rhs)),m)
    case t@chalice.PermTimes(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(permissionMultiplication,
        TermSequence(translatePermission(lhs),translatePermission(rhs)),t)
    case t@chalice.IntPermTimes(factor,perm) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(permissionIntegerMultiplication,
        TermSequence(translateTerm(factor),translatePermission(perm)),t)
    case f@chalice.Full => currentExpressionFactory.makeFullPermission(f)
    case f@chalice.Frac(chalice.IntLiteral(100)) => currentExpressionFactory.makeFullPermission(f)
    case f@chalice.Frac(n) => currentExpressionFactory.makePercentagePermission(translateTerm(n),f)
    case k@chalice.Epsilon => readFraction(k)
    case k@chalice.ForkEpsilon(token) =>
      assert(token.typ.IsToken,"Expected type of operand in rd(token) to be a token.")
      val calleeFactory = methods(token.typ.asInstanceOf[chalice.TokenClass].method)
      currentExpressionFactory.makeFieldReadTerm(translateTerm(token), calleeFactory.callToken.readFraction,k)
    case k@chalice.MethodEpsilon => readFraction(k)    //only one of these three is relevant at any given time
    case k@chalice.PredicateEpsilon(Some(e@chalice.MemberAccess(ref,_))) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Predicate().readFraction,TermSequence(
          currentExpressionFactory.makeIntegerLiteralTerm(predicates(e.predicate).id,k),
          translateTerm(ref)
      ),k)
    case k@chalice.PredicateEpsilon(None) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Predicate().globalReadFraction,TermSequence(),k)
    case k@chalice.MonitorEpsilon(Some(ref)) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Monitor().readFraction,
        TermSequence(translateTerm(ref)),k)
    case k@chalice.MonitorEpsilon(None) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Monitor().globalReadFraction,TermSequence(),k)
    case e@chalice.Epsilons(intExpr) =>
         currentExpressionFactory.makeDomainFunctionApplicationTerm(
           permissionIntegerMultiplication,TermSequence(translateTerm(intExpr), currentExpressionFactory.makeEpsilonPermission(e)),e)
    case permission =>
      report(messages.UnknownAstNode(permission))
      currentExpressionFactory.makeNoPermission(permission)

  }

  protected def readFraction(location : SourceLocation) : Term = {
    report(messages.NoContextForReadPermission(location))
    currentExpressionFactory.makeNoPermission(location)
  }
}
