package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import silAST.programs.symbols.ProgramVariable
import silAST.ASTNode
import silAST.source.SourceLocation
import silAST.expressions._
import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import terms._
import translation.{MemberEnvironment, DerivedMemberEnvironment}
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
      currentExpressionFactory.makeBinaryExpression(op, transplant(lhs), transplant(rhs),expression)
    case UnaryExpression(op, expr) =>
      currentExpressionFactory.makeUnaryExpression(op, transplant(expr),expression)
    case EqualityExpression(lhs, rhs) =>
      currentExpressionFactory.makeEqualityExpression(transplant(lhs), transplant(rhs),expression)
    case DomainPredicateExpression(p, args) =>
      currentExpressionFactory.makeDomainPredicateExpression(p, transplant(args),expression)
    case PermissionExpression(FieldLocation(receiver,field), perm) =>
      currentExpressionFactory.makeFieldPermissionExpression(transplant(receiver), field, transplant(perm),expression)
    case PermissionExpression(PredicateLocation(receiver,predicate), perm) =>
      currentExpressionFactory.makePredicatePermissionExpression(transplant(receiver), predicates.lookup(predicate.name), transplant(perm),expression)
    case UnfoldingExpression(PredicateLocation(receiver,predicate), perm, expr) =>
      currentExpressionFactory.makeUnfoldingExpression(transplant(receiver),predicates.lookup(predicate.name),transplant(perm), transplant(expr),expression)
    case QuantifierExpression(q,v,inner) =>
      val logical = currentExpressionFactory.makeBoundVariable(v.name,v.dataType,expression)
      val old = translateLogicalVariable
      translateLogicalVariable += v -> logical
      val r = currentExpressionFactory.makeQuantifierExpression(q,logical, transplant(inner))(expression)
      translateLogicalVariable = old
      r
    case t@TrueExpression() => t
    case t@FalseExpression() => t
    case _ =>
      report(messages.ContractNotUnderstood(expression))
      dummyExpr(currentExpressionFactory, expression)
  }

  def transplant(term : Term) : Term = term match {
    case CastTerm(operand, newType) => currentExpressionFactory.makeCastTerm(transplant(operand), newType,term)
    case FieldReadTerm(FieldLocation(receiver,field)) => currentExpressionFactory.makeFieldReadTerm(transplant(receiver),field,term)
    case DomainFunctionApplicationTerm(f, args) => currentExpressionFactory.makeDomainFunctionApplicationTerm(f, transplant(args),term)
    case EpsilonPermissionTerm() => currentExpressionFactory.makeEpsilonPermission(term)
    case FullPermissionTerm() => currentExpressionFactory.makeFullPermission(term)
    case FunctionApplicationTerm(receiver, f, args) => currentExpressionFactory.makeFunctionApplicationTerm(transplant(receiver),
      functions.lookup(f.name), transplant(args),term)
    case NoPermissionTerm() => currentExpressionFactory.makeNoPermission(term)
    case UnfoldingTerm(PredicateLocation(receiver,predicate), perm, body) =>
      currentExpressionFactory.makeUnfoldingTerm(transplant(receiver),predicates.lookup(predicate.name), transplant(perm), transplant(body),term)
    case PermTerm(FieldLocation(receiver,field)) => currentExpressionFactory.makePermTerm(transplant(receiver),field)(term)
    case ProgramVariableTerm(v) => translateProgramVariable(v)
    case LogicalVariableTerm(v) =>
      val v2 = translateLogicalVariable(v)
      currentExpressionFactory.makeBoundVariableTerm(v2,term.sourceLocation)
    case i : IntegerLiteralTerm => currentExpressionFactory.makeIntegerLiteralTerm(i.value,term)
    case _ =>
      report(messages.ContractNotUnderstood(term))
      currentExpressionFactory.makeIntegerLiteralTerm(67,term)
  }

  def transplant(terms : TermSequence) : TermSequence = TermSequence(terms.map(transplant(_)) : _*)
}
