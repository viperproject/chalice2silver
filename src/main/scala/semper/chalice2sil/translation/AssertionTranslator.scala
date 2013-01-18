package semper.chalice2sil.translation

import semper.chalice2sil._

import semper.sil.ast.expressions.util.ExpressionSequence
import semper.sil.ast.types.permissionLE
import semper.sil.ast.expressions.Expression

/**
  * Using the assertion translator, any chalice expression can be `assert` ed or `assume` ed via
  * `exhale` and `inhale`, respectively, without changing the permissions held at the moment.
  * @author Christian Klauser
  */
trait AssertionTranslator extends ExpressionTranslator {
  override protected def expressionTranslation = matchingExpression {
    case a@chalice.Access(ma@chalice.MemberAccess(location,_),permission) =>
      val locationExpression = translateExpression(location)
      val field = fields(ma.f)
      val currentPermission = currentExpressionFactory.makePermExpression(locationExpression, field)(a)
      permission match {
        /*case chalice.Epsilon =>
          // 0 < perm(location,field)
          val noPermission = currentExpressionFactory.makeNoPermission(a)
          currentExpressionFactory.makeDomainPredicateExpression(
            permissionLT,ExpressionSequence(noPermission,currentPermission),a) */
        case _ =>
          // $REQUIRED$ ≤ perm(objRef,field)
          val requiredPermission = translatePermission(permission)
          currentExpressionFactory.makeDomainPredicateExpression(
            permissionLE,ExpressionSequence(requiredPermission,currentPermission),a)
      }
  } orElse super.expressionTranslation
}
