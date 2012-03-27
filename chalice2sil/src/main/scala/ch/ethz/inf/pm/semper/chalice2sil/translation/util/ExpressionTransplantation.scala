package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import silAST.programs.symbols.ProgramVariable
import silAST.ASTNode
import silAST.source.SourceLocation
import silAST.expressions._
import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import terms._
import translation.{MemberEnvironment, DerivedMemberEnvironment, ProgramEnvironment}
import silAST.expressions.util.TermSequence
import collection.immutable
import silAST.symbols.logical.quantification.LogicalVariable

/**
  * @author Christian Klauser
  */
abstract class ExpressionTransplantation(methodEnvironment : MemberEnvironment)
  extends DerivedMemberEnvironment(methodEnvironment) {
  implicit def extractSourceLocation(node : ASTNode) : SourceLocation = node.sourceLocation

  def translateProgramVariable(variable : ProgramVariable) : PTerm

  var translateLogicalVariable : immutable.Map[LogicalVariable,LogicalVariable] = Map()

  def transplant(expression : Expression) : Expression = expression match {
    case BinaryExpression(op, lhs, rhs) =>
      currentExpressionFactory.makeBinaryExpression(expression, op, transplant(lhs), transplant(rhs))
    case UnaryExpression(op, expr) =>
      currentExpressionFactory.makeUnaryExpression(expression, op, transplant(expr))
    case EqualityExpression(lhs, rhs) =>
      currentExpressionFactory.makeEqualityExpression(expression, transplant(lhs), transplant(rhs))
    case DomainPredicateExpression(p, args) =>
      currentExpressionFactory.makeDomainPredicateExpression(expression, p, transplant(args))
    case PermissionExpression(ref, field, perm) =>
      currentExpressionFactory.makePermissionExpression(expression, transplant(ref), field, transplant(perm))
    case UnfoldingExpression(p, expr) =>
      currentExpressionFactory.makeUnfoldingExpression(expression, transplantPredicateExpression(p), transplant(expr))
    case PredicateExpression(receiver, predicate) =>
      currentExpressionFactory.makePredicateExpression(expression, transplant(receiver), predicates.lookup(predicate.name))
    case QuantifierExpression(q,v,inner) =>
      val logical = currentExpressionFactory.makeBoundVariable(expression,v.name,v.dataType)
      val old = translateLogicalVariable
      translateLogicalVariable += v -> logical
      val r = currentExpressionFactory.makeQuantifierExpression(expression,q,logical, transplant(inner))
      translateLogicalVariable = old
      r
    case t@TrueExpression() => t
    case t@FalseExpression() => t
    case _ =>
      report(messages.ContractNotUnderstood(expression))
      dummyExpr(currentExpressionFactory, expression)
  }

  protected def transplantPredicateExpression(predicateExpression : PredicateExpression) : PredicateExpression = {
    currentExpressionFactory.makePredicateExpression(
      predicateExpression,
      transplant(predicateExpression.receiver),
      predicates.lookup(predicateExpression.predicate.name))
  }

  def transplant(term : Term) : Term = term match {
    case CastTerm(operand, newType) => currentExpressionFactory.makeCastTerm(term, transplant(operand), newType)
    case FieldReadTerm(location, field) => currentExpressionFactory.makeFieldReadTerm(term, transplant(location), field)
    case DomainFunctionApplicationTerm(f, args) => currentExpressionFactory.makeDomainFunctionApplicationTerm(term, f, transplant(args))
    case EpsilonPermissionTerm() => currentExpressionFactory.makeEpsilonPermission(term)
    case FullPermissionTerm() => currentExpressionFactory.makeFullPermission(term)
    case FunctionApplicationTerm(receiver, f, args) => currentExpressionFactory.makeFunctionApplicationTerm(term, transplant(receiver),
      functions.lookup(f.name), transplant(args))
    case NoPermissionTerm() => currentExpressionFactory.makeNoPermission(term)
    case UnfoldingTerm(receiver, p, body) => currentExpressionFactory.makeUnfoldingTerm(term, transplant(receiver), predicates.lookup(p.name), transplant(body))
    case PermTerm(location, field) => currentExpressionFactory.makePermTerm(term, transplant(location), field)
    case ProgramVariableTerm(v) => translateProgramVariable(v)
    case LogicalVariableTerm(v) =>
      val v2 = translateLogicalVariable(v)
      currentExpressionFactory.makeBoundVariableTerm(term.sourceLocation,v2)
    case i : IntegerLiteralTerm => currentExpressionFactory.makeIntegerLiteralTerm(term, i.value)
    case _ =>
      report(messages.ContractNotUnderstood(term))
      currentExpressionFactory.makeIntegerLiteralTerm(term, 67)
  }

  def transplant(terms : TermSequence) : TermSequence = TermSequence(terms.map(transplant(_)) : _*)
}
