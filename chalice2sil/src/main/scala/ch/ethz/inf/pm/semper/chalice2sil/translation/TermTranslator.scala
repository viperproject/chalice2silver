package ch.ethz.inf.pm.semper.chalice2sil.translation

import operators.Lookup.Success
import silAST.source.SourceLocation
import math.BigInt._
import ch.ethz.inf.pm.semper.chalice2sil._
import silAST.types.nullFunction
import silAST.expressions.util.{PTermSequence, TermSequence}
import silAST.expressions.terms.{PTerm, Term}
import silAST.programs.symbols.PredicateFactory
import silAST.expressions.{PPredicatePermissionExpression, PredicatePermissionExpression}

/**
  * @author Christian Klauser
  */
trait TermTranslator extends MemberEnvironment with TypeTranslator {

  def translateTerm(expression : chalice.Expression) : Term = termTranslation(expression)

  protected def matchingTerm(partialFunction : PartialFunction[chalice.Expression, Term]) = partialFunction
  
  def dummyTerm(location : SourceLocation) = currentExpressionFactory.makeIntegerLiteralTerm(27,location)

  protected def termTranslation : PartialFunction[chalice.Expression,Term] = matchingTerm {
    case rvalue@chalice.IntLiteral(i) =>
      currentExpressionFactory.makeIntegerLiteralTerm(i,rvalue)
    case rvalue@chalice.BoolLiteral(true) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Boolean.trueLiteral,TermSequence(),rvalue)
    case rvalue@chalice.BoolLiteral(false) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Boolean.falseLiteral,TermSequence(),rvalue)
    case variableExpr:chalice.VariableExpr =>
      currentExpressionFactory.makeProgramVariableTerm((programVariables(variableExpr.v)),variableExpr)
    case rvalue@chalice.Old(e) => currentExpressionFactory.makeOldTerm(translateTerm(e))(rvalue)
    case access@chalice.MemberAccess(rcvr,_) if !access.isPredicate =>
      assert(access.f != null,"Chalice MemberAccess node (%s) is not linked to a field.".format(access))
      val rcvrTerm = translateTerm(rcvr)
      currentExpressionFactory.makeFieldReadTerm(rcvrTerm,fields(access.f),access)
    case ifThenElse@chalice.IfThenElse(cond,thn,els) =>
      val condTerm = translateTerm(cond)
      val thnTerm = translateTerm(thn)
      val elsTerm = translateTerm(els)
      currentExpressionFactory.makeIfThenElseTerm(condTerm,thnTerm,elsTerm)(ifThenElse)
    case th@chalice.ImplicitThisExpr() =>
      currentExpressionFactory.makeProgramVariableTerm(thisVariable,th)
    case th@chalice.ExplicitThisExpr() =>
      currentExpressionFactory.makeProgramVariableTerm(thisVariable,th)
    case rvalue@chalice.And(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Boolean.logicalAnd,TermSequence(
        translateTerm(lhs),
        translateTerm(rhs)
      ),rvalue)
    case rvalue@chalice.Or(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Boolean.logicalOr,TermSequence(
        translateTerm(lhs),
        translateTerm(rhs)
      ),rvalue)
    case rvalue@chalice.Implies(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Boolean.implication,TermSequence(
        translateTerm(lhs),
        translateTerm(rhs)
      ),rvalue)
    case rvalue@chalice.Not(op) =>
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Boolean.not,TermSequence(
        translateTerm(op)
      ),rvalue)
    case literal@chalice.NullLiteral() =>
      currentExpressionFactory.makePDomainFunctionApplicationTerm(nullFunction,PTermSequence(),literal)
    case unfolding@chalice.Unfolding(predicateAccess, body) =>
      val location = translateTerm(predicateAccess.ma.e)
      val predicateExpression = makePredicatePermissionExpression(location,
        predicates(predicateAccess.ma.predicate),
        translatePermission(predicateAccess.perm),unfolding)
      (predicateExpression,translateTerm(body)) match {
        case (pred:PPredicatePermissionExpression,bodyTerm:PTerm) =>
          currentExpressionFactory.makePUnfoldingTerm(pred,bodyTerm,unfolding)
        case (_,bodyTerm) =>
          currentExpressionFactory.makeUnfoldingTerm(predicateExpression,bodyTerm,unfolding)
      }
    case functionApplication@chalice.FunctionApplication(receiver,_,args) =>
      currentExpressionFactory.makeFunctionApplicationTerm(
        translateTerm(receiver),
        functions(functionApplication.f),
        TermSequence(args.map(translateTerm(_)):_*),functionApplication)
    case binary:chalice.BinaryExpr => translateBinaryExpression(binary)
  }

  def makePredicatePermissionExpression(location : Term, predicateFactory : PredicateFactory,permission : Term, sourceLocation : SourceLocation ) : PredicatePermissionExpression =
    (location,permission) match {
    case (pLocation : PTerm,pPermission : PTerm) => currentExpressionFactory.makePPredicatePermissionExpression(pLocation,predicateFactory,pPermission,sourceLocation)
    case _ => currentExpressionFactory.makePredicatePermissionExpression(location,predicateFactory,permission,sourceLocation)
  }

  protected def translateBinaryExpression(binary : chalice.BinaryExpr) : Term = {
    val (lhs,rhs) = (binary.E0,binary.E1)
    // binary.ExpectedXhsType is often null, use the "inferred" types for the operands instead
    val (lhsType,rhsType,resultType) = (translateClassRef(lhs.typ),translateClassRef(rhs.typ),translateClassRef(binary.ResultType))

    domainFunctionLookup.lookup(List(lhsType,rhsType,resultType).map(_.domain))(binary.OpName,List(Some(lhsType),Some(rhsType))) match {
      case Success(e) => currentExpressionFactory.makeDomainFunctionApplicationTerm(e,TermSequence(translateTerm(lhs),translateTerm(rhs)),binary)
      case _ if binary.OpName == "!=" => // If no NEQ operator exists, translate as ¬(_ == _)
        val eq = chalice.Eq(binary.E0,binary.E1)
        eq.typ = binary.ResultType
        eq.pos = binary.pos
        val neg = chalice.Not(eq)
        neg.typ = binary.ResultType
        neg.pos = binary.pos
        translateTerm(neg)
      case _ =>
        report(messages.OperatorNotFound(binary,lhsType,rhsType,resultType))
        dummyTerm(binary)
    }
  }

  def translatePermission(permission : chalice.Permission) : Term

}
