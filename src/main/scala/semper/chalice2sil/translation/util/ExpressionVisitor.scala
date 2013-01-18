package semper.chalice2sil.translation.util

import semper.chalice2sil
import chalice2sil._
import semper.sil.ast.expressions._
import terms._


/**
 * @author Christian Klauser
 * @tparam A type of visit arguments
 * @tparam R type of visit results
 */
trait ExpressionVisitor[A, R] {

  import semper.sil.ast.expressions.PermissionExpression

  def visitExpression(expression: Expression, arg: A): R = expression match {
    case BinaryExpression(op, lhs, rhs) =>
      visitMergeExpressions(arg, lhs, rhs)
    case UnaryExpression(op, expr) =>
      visitExpression(expr, arg)
    case EqualityExpression(lhs, rhs) =>
      visitMergeExpressions(arg, lhs, rhs)
    case DomainPredicateExpression(p, args) =>
      visitMergeExpressions(arg, args: _*)
    case PermissionExpression(location, perm) =>
      visitMergeExpressions(arg, getReceiverFromLocation(location), perm)
    case QuantifierExpression(q, v, expr) => visitExpression(expr, arg)
    case TrueExpression()
         | FalseExpression() => zero
    case OldExpression(inner) => visitExpression(inner, arg)
    case CastExpression(operand, newType) => visitExpression(operand, arg)
    case FieldReadExpression(location) => visitExpression(getReceiverFromLocation(location), arg)
    case DomainFunctionApplicationExpression(f, args) => visitMergeExpressions(arg, args: _*)
    case EpsilonPermissionExpression() => zero
    case FullPermissionExpression() => zero
    case FunctionApplicationExpression(receiver, f, args) => merge(visitExpression(receiver, arg), visitMergeExpressions(arg, args: _*))
    case NoPermissionExpression() => zero
    case UnfoldingExpression(PredicatePermissionExpression(location, perm), body) => visitMergeExpressions(arg, getReceiverFromLocation(location), perm, body)
    case PermExpression(location) => visitExpression(getReceiverFromLocation(location), arg)
    case ProgramVariableExpression(v) => zero
    case i: IntegerLiteralExpression => zero
    case IfThenElseExpression(cond, then, otherwise) => visitMergeExpressions(arg, cond, then, otherwise)
    case LogicalVariableExpression(v) => zero
  }

  private def visitMergeExpressions(arg: A, xs: Expression*): R = mergeMany(xs.map(visitExpression(_, arg)))

  protected def merge(left: R, right: R): R

  protected def zero: R

  protected def mergeMany(rs: Traversable[R]): R = rs.fold(zero)(merge)
}
