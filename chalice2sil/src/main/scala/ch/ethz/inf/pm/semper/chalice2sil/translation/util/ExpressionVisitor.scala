package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import silAST.expressions._
import terms._


/**
  * @author Christian Klauser
  * @tparam A type of visit arguments
  * @tparam R type of visit results
  */
trait ExpressionVisitor[A, R] {
  import silAST.expressions.PermissionExpression

  def visitExpression(expression : Expression, arg : A) : R = expression match {
    case BinaryExpression(op, lhs, rhs) =>
      visitMergeExpressions(arg, lhs, rhs)
    case UnaryExpression(op, expr) =>
      visitExpression(expr, arg)
    case EqualityExpression(lhs, rhs) =>
      visitMergeTerms(arg, lhs, rhs)
    case DomainPredicateExpression(p, args) =>
      visitMergeTerms(arg, args : _*)
    case PermissionExpression(location,perm) =>
      visitMergeTerms(arg, getReceiverFromLocation(location),perm)
    case UnfoldingExpression(PredicatePermissionExpression(location, perm), expr) =>
      merge(
        visitMergeTerms(arg, getReceiverFromLocation(location),perm),
        visitExpression(expr,arg))
    case QuantifierExpression(q, v, expr) => visitExpression(expr, arg)
    case TrueExpression()
         | FalseExpression() => zero
    case OldExpression(inner) => visitExpression(inner, arg)
  }

  def visitTerm(term : Term, arg : A) : R = term match {
    case CastTerm(operand, newType) => visitTerm(operand, arg)
    case FieldReadTerm(location) => visitTerm(getReceiverFromLocation(location), arg)
    case DomainFunctionApplicationTerm(f, args) => visitMergeTerms(arg, args : _*)
    case EpsilonPermissionTerm() => zero
    case FullPermissionTerm() => zero
    case FunctionApplicationTerm(receiver, f, args) => merge(visitTerm(receiver, arg), visitMergeTerms(arg, args : _*))
    case NoPermissionTerm() => zero
    case UnfoldingTerm(PredicatePermissionExpression(location,perm), body) => visitMergeTerms(arg, getReceiverFromLocation(location),perm, body)
    case PermTerm(location) => visitTerm(getReceiverFromLocation(location), arg)
    case ProgramVariableTerm(v) => zero
    case i : IntegerLiteralTerm => zero
    case IfThenElseTerm(cond, then, otherwise) => visitMergeTerms(arg, cond, then, otherwise)
    case LogicalVariableTerm(v) => zero
    case OldTerm(t) => visitTerm(term, arg)
  }

  private def visitMergeExpressions(arg : A, xs : Expression*) : R = mergeMany(xs.map(visitExpression(_, arg)))

  private def visitMergeTerms(arg : A, xs : Term*) : R = mergeMany(xs.map(visitTerm(_, arg)))

  protected def merge(left : R, right : R) : R

  protected def zero : R

  protected def mergeMany(rs : Traversable[R]) : R = rs.fold(zero)(merge)
}
