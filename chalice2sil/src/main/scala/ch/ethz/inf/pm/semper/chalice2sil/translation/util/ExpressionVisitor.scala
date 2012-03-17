package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import silAST.expressions._
import terms._


/**
  * @author Christian Klauser
  * @tparam A type of visit arguments
  * @tparam R type of visit results
  */
abstract class ExpressionVisitor[A, R] {
  def visitExpression(expression : Expression, arg : A) : R = expression match {
    case BinaryExpression(op, lhs, rhs) =>
      visitMergeExpressions(arg, lhs, rhs)
    case UnaryExpression(op, expr) =>
      visitExpression(expr, arg)
    case EqualityExpression(lhs, rhs) =>
      visitMergeTerms(arg, lhs, rhs)
    case DomainPredicateExpression(p, args) =>
      visitMergeTerms(arg, args : _*)
    case PermissionExpression(ref, field, perm) =>
      visitMergeTerms(arg, ref, perm)
    case UnfoldingExpression(p, expr) =>
      visitMergeExpressions(arg, p, expr)
    case PredicateExpression(receiver, predicate) =>
      visitTerm(receiver, arg)
    case QuantifierExpression(q, v, expr) => visitExpression(expr, arg)
    case TrueExpression()
         | FalseExpression() => zero
    case OldExpression(inner) => visitExpression(inner, arg)
  }

  def visitTerm(term : Term, arg : A) : R = term match {
    case CastTerm(operand, newType) => visitTerm(operand, arg)
    case FieldReadTerm(location, field) => visitTerm(location, arg)
    case DomainFunctionApplicationTerm(f, args) => visitMergeTerms(arg, args : _*)
    case EpsilonPermissionTerm() => zero
    case FullPermissionTerm() => zero
    case FunctionApplicationTerm(receiver, f, args) => merge(visitTerm(receiver, arg), visitMergeTerms(arg, args : _*))
    case NoPermissionTerm() => zero
    case UnfoldingTerm(receiver, p, body) => visitMergeTerms(arg, receiver, body)
    case PermTerm(location, field) => visitTerm(location, arg)
    case ProgramVariableTerm(v) => zero
    case i : IntegerLiteralTerm => zero
    case IfThenElseTerm(cond, then, otherwise) => visitMergeTerms(arg, cond, then, otherwise)
  }

  private def visitMergeExpressions(arg : A, xs : Expression*) : R = mergeMany(xs.map(visitExpression(_, arg)))

  private def visitMergeTerms(arg : A, xs : Term*) : R = mergeMany(xs.map(visitTerm(_, arg)))

  protected def merge(left : R, right : R) : R

  protected def zero : R

  protected def mergeMany(rs : Traversable[R]) : R = rs.fold(zero)(merge)
}
