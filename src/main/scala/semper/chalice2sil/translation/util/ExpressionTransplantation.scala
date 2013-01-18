package semper.chalice2sil.translation.util

import semper.sil.ast.programs.symbols.ProgramVariable
import semper.sil.ast.ASTNode
import semper.sil.ast.source.SourceLocation
import semper.sil.ast.expressions._
import semper.chalice2sil
import chalice2sil._
import terms._
import translation.{MemberEnvironment, DerivedMemberEnvironment}
import semper.sil.ast.expressions.util.ExpressionSequence
import collection.immutable
import semper.sil.ast.symbols.logical.quantification.LogicalVariable

/**
 * @author Christian Klauser
 */
abstract class ExpressionTransplantation(methodEnvironment: MemberEnvironment)
  extends DerivedMemberEnvironment(methodEnvironment) {
  implicit def extractSourceLocation(node: ASTNode): SourceLocation = node.sourceLocation

  def translateProgramVariable(variable: ProgramVariable): Expression

  var translateLogicalVariable: immutable.Map[LogicalVariable, LogicalVariable] = Map()

  def transplant(expression: Expression): Expression = expression match {
    case BinaryExpression(op, lhs, rhs) =>
      currentExpressionFactory.makeBinaryExpression(op, transplant(lhs), transplant(rhs), expression)
    case UnaryExpression(op, expr) =>
      currentExpressionFactory.makeUnaryExpression(op, transplant(expr), expression)
    case EqualityExpression(lhs, rhs) =>
      currentExpressionFactory.makeEqualityExpression(transplant(lhs), transplant(rhs), expression)
    case DomainPredicateExpression(p, args) =>
      currentExpressionFactory.makeDomainPredicateExpression(p, transplant(args), expression)
    case PermissionExpression(FieldLocation(receiver, field), perm) =>
      currentExpressionFactory.makeFieldPermissionExpression(transplant(receiver), field, transplant(perm), expression)
    case PermissionExpression(PredicateLocation(receiver, predicate), perm) =>
      currentExpressionFactory.makePredicatePermissionExpression(transplant(receiver), predicates.lookup(predicate.name), transplant(perm), expression)
    case UnfoldingExpression(PredicatePermissionExpression(PredicateLocation(receiver, predicate), perm), expr) =>
      val predicateAccess = currentExpressionFactory.makePredicatePermissionExpression(transplant(receiver), predicates.lookup(predicate.name), transplant(perm), expression)
      currentExpressionFactory.makeUnfoldingExpression(predicateAccess, transplant(expr), expression)
    case QuantifierExpression(q, v, inner) =>
      val logical = currentExpressionFactory.makeBoundVariable(v.name, v.dataType, expression)
      val old = translateLogicalVariable
      translateLogicalVariable += v -> logical
      val r = currentExpressionFactory.makeQuantifierExpression(q, logical, transplant(inner))(expression)
      translateLogicalVariable = old
      r
    case t@TrueExpression() => t
    case t@FalseExpression() => t
    case CastExpression(operand, newType) => currentExpressionFactory.makeCastExpression(transplant(operand), newType, expression)
    case FieldReadExpression(FieldLocation(receiver, field)) => currentExpressionFactory.makeFieldReadExpression(transplant(receiver), field, expression)
    case DomainFunctionApplicationExpression(f, args) => currentExpressionFactory.makeDomainFunctionApplicationExpression(f, transplant(args), expression)
    case EpsilonPermissionExpression() => currentExpressionFactory.makeEpsilonPermission(expression)
    case FullPermissionExpression() => currentExpressionFactory.makeFullPermission(expression)
    case FunctionApplicationExpression(receiver, f, args) => currentExpressionFactory.makeFunctionApplicationExpression(transplant(receiver),
      functions.lookup(f.name), transplant(args), expression)
    case NoPermissionExpression() => currentExpressionFactory.makeNoPermission(expression)
    case PermExpression(FieldLocation(receiver, field)) => currentExpressionFactory.makePermExpression(transplant(receiver), field)(expression)
    case ProgramVariableExpression(v) => translateProgramVariable(v)
    case LogicalVariableExpression(v) =>
      val v2 = translateLogicalVariable(v)
      currentExpressionFactory.makeBoundVariableExpression(v2, expression.sourceLocation)
    case i: IntegerLiteralExpression => currentExpressionFactory.makeIntegerLiteralExpression(i.value, expression)
    case _ =>
      report(messages.ContractNotUnderstood(expression))
      dummyExpr(currentExpressionFactory, expression)
  }

  def transplant(terms: ExpressionSequence): ExpressionSequence = ExpressionSequence(terms.map(transplant(_)): _*)
}
