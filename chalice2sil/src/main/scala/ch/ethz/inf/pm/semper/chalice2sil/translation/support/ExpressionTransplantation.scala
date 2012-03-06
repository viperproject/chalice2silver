package ch.ethz.inf.pm.semper.chalice2sil.translation.support

import silAST.programs.symbols.ProgramVariable
import silAST.ASTNode
import silAST.source.SourceLocation
import silAST.expressions._
import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import chalice2sil.translation.ProgramEnvironment
import terms._
import util.TermSequence

/**
  * @author Christian Klauser
  */
abstract class ExpressionTransplantation(val expressionFactory : ExpressionFactory) extends ProgramEnvironment {
  implicit def extractSourceLocation(node : ASTNode) : SourceLocation = node.sourceLocation 
  
  def translateProgramVariable(variable : ProgramVariable) : PTerm
  
  def transplant(expression : Expression) : Expression = expression match {
    case BinaryExpression(op,lhs,rhs) =>
      expressionFactory.makeBinaryExpression(expression,op,transplant(lhs),transplant(rhs))
    case UnaryExpression(op,expr) =>
      expressionFactory.makeUnaryExpression(expression,op,transplant(expr))
    case EqualityExpression(lhs,rhs) =>
      expressionFactory.makeEqualityExpression(expression,transplant(lhs),transplant(rhs))
    case DomainPredicateExpression(p,args) =>
      expressionFactory.makeDomainPredicateExpression(expression,p,args)
    case PermissionExpression(ref,field,perm) =>
      expressionFactory.makePermissionExpression(expression,transplant(ref),field,transplant(perm))
    case UnfoldingExpression(p,expr) =>
      expressionFactory.makeUnfoldingExpression(expression,transplantPredicateExpression(p),transplant(expr))
    case PredicateExpression(receiver,predicate) =>
      expressionFactory.makePredicateExpression(expression, transplant(receiver),  predicates.lookup(predicate.name))
    case _ => 
      report(messages.ContractNotUnderstood(expression))
      dummyExpr(expressionFactory,expression)
  }

  protected def transplantPredicateExpression(predicateExpression : PredicateExpression) : PredicateExpression = {
    expressionFactory.makePredicateExpression(
      predicateExpression,
      transplant(predicateExpression.receiver),
      predicates.lookup(predicateExpression.predicate.name))
  }

  def transplant(term : Term) : Term = term match {
    case CastTerm(operand,newType) => expressionFactory.makeCastTerm(term,transplant(operand),newType)
    case FieldReadTerm(location,field) => expressionFactory.makeFieldReadTerm(term, transplant(location),field)
    case DomainFunctionApplicationTerm(f,args) => expressionFactory.makeDomainFunctionApplicationTerm(term,f,transplant(args))
    case EpsilonPermissionTerm() => expressionFactory.makeEpsilonPermission(term)
    case FullPermissionTerm() => expressionFactory.makeFullPermission(term)
    case FunctionApplicationTerm(receiver,f,args) => expressionFactory.makeFunctionApplicationTerm(term,transplant(receiver),
      functions.lookup(f.name),transplant(args))
    case NoPermissionTerm() => expressionFactory.makeNoPermission(term)
    case UnfoldingTerm(receiver, p, body) => expressionFactory.makeUnfoldingTerm(term,transplant(receiver),predicates.lookup(p.name),transplant(body))
    case PermTerm(location,field) => expressionFactory.makePermTerm(term,transplant(location),field)
    case ProgramVariableTerm(v) => translateProgramVariable(v)
    case i:IntegerLiteralTerm => expressionFactory.makeIntegerLiteralTerm(term,i.value)
    case _ => 
      report(messages.ContractNotUnderstood(term))
      expressionFactory.makeIntegerLiteralTerm(term,67)
  }
  
  protected def transplant(terms : TermSequence) : TermSequence = TermSequence(terms.map(transplant(_)):_*)
}
