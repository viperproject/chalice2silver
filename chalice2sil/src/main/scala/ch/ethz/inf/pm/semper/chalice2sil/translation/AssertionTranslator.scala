package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil._

import silAST.expressions.Expression
import silAST.expressions.util.TermSequence
import silAST.types.{permissionGT, permissionLE, permissionLT}
import silAST.symbols.logical.And

/**
  * Using the assertion translator, any chalice expression can be `assert` ed or `assume` ed via
  * `exhale` and `inhale`, respectively, without changing the permissions held at the moment.
  * @author Christian Klauser
  */
trait AssertionTranslator extends ExpressionTranslator {
  override protected def expressionTranslation = matchingExpression {
    case a@chalice.Access(ma@chalice.MemberAccess(location,_),permission) =>
      val locationTerm = translateTerm(location)
      val field = fields(ma.f)
      val currentPermission = currentExpressionFactory.makePermTerm(a, locationTerm, field)
      permission match {
        case chalice.Epsilon =>
          // 0 < perm(location,field)
          val noPermission = currentExpressionFactory.makeNoPermission(a)
          currentExpressionFactory.makeDomainPredicateExpression(a,
            permissionLT,TermSequence(noPermission,currentPermission))
        case _ =>
          // $REQUIRED$ ≤ perm(objRef,field)
          val requiredPermission = translatePermission(permission)
          currentExpressionFactory.makeDomainPredicateExpression(a,
            permissionLE,TermSequence(requiredPermission,currentPermission))
      }
  } orElse super.expressionTranslation
}
