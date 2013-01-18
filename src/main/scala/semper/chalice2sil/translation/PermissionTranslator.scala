package semper.chalice2sil.translation

import semper.chalice2sil._

import semper.sil.ast.source.SourceLocation
import semper.sil.ast.expressions.util.ExpressionSequence
import semper.sil.ast.types.{permissionAddition, permissionSubtraction, permissionMultiplication, permissionIntegerMultiplication}
import semper.sil.ast.expressions.Expression

/**
  * @author Christian Klauser
  */
trait PermissionTranslator extends TermTranslator {
  
  def translatePermission(permission : chalice.Permission) : Expression = permissionTranslation(permission)

  protected def matchingPermission(partialFunction : PartialFunction[chalice.Permission, Expression]) = partialFunction

  protected def permissionTranslation = matchingPermission {
    case p@chalice.PermPlus(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(permissionAddition,
        ExpressionSequence(translatePermission(lhs),translatePermission(rhs)),p)
    case m@chalice.PermMinus(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(permissionSubtraction,
        ExpressionSequence(translatePermission(lhs),translatePermission(rhs)),m)
    case t@chalice.PermTimes(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(permissionMultiplication,
        ExpressionSequence(translatePermission(lhs),translatePermission(rhs)),t)
    case t@chalice.IntPermTimes(factor,perm) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(permissionIntegerMultiplication,
        ExpressionSequence(translateExpression(factor),translatePermission(perm)),t)
    case f@chalice.Full => currentExpressionFactory.makeFullPermission(f)
    case f@chalice.Frac(chalice.IntLiteral(100)) => currentExpressionFactory.makeFullPermission(f)
    case f@chalice.Frac(n) => currentExpressionFactory.makePercentagePermission(translateExpression(n),f)
    case k@chalice.Epsilon => readFraction(k)
    case k@chalice.ForkEpsilon(token) =>
      assert(token.typ.IsToken,"Expected type of operand in rd(token) to be a token.")
      val calleeFactory = methods(token.typ.asInstanceOf[chalice.TokenClass].method)
      currentExpressionFactory.makeFieldReadExpression(translateExpression(token), calleeFactory.callToken.readFraction,k)
    case k@chalice.MethodEpsilon => readFraction(k)    //only one of these three is relevant at any given time
    case k@chalice.PredicateEpsilon(Some(e@chalice.MemberAccess(ref,_))) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Predicate().readFraction,ExpressionSequence(
          currentExpressionFactory.makeIntegerLiteralExpression(predicates(e.predicate).id,k),
          translateExpression(ref)
      ),k)
    case k@chalice.PredicateEpsilon(None) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Predicate().globalReadFraction,ExpressionSequence(),k)
    case k@chalice.MonitorEpsilon(Some(ref)) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Monitor().readFraction,
        ExpressionSequence(translateExpression(ref)),k)
    case k@chalice.MonitorEpsilon(None) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Monitor().globalReadFraction,ExpressionSequence(),k)
    case e@chalice.Epsilons(intExpr) =>
         currentExpressionFactory.makeDomainFunctionApplicationExpression(
           permissionIntegerMultiplication,ExpressionSequence(translateExpression(intExpr), currentExpressionFactory.makeEpsilonPermission(e)),e)
    case permission =>
      report(messages.UnknownAstNode(permission))
      currentExpressionFactory.makeNoPermission(permission)

  }

  protected def readFraction(location : SourceLocation) : Expression = {
    report(messages.NoContextForReadPermission(location))
    currentExpressionFactory.makeNoPermission(location)
  }
}
