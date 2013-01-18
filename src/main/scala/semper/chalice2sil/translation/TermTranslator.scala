package semper.chalice2sil.translation

import operators.Lookup.Success
import semper.sil.ast.source.SourceLocation
import math.BigInt._
import semper.chalice2sil._
import semper.sil.ast.types.nullFunction
import semper.sil.ast.expressions.util.ExpressionSequence
import semper.sil.ast.programs.symbols.PredicateFactory
import semper.sil.ast.expressions.{Expression, PredicatePermissionExpression}
import semper.sil.ast.symbols.logical.quantification.LogicalVariable

/**
  * @author Christian Klauser
  */
trait TermTranslator extends MemberEnvironment with TypeTranslator { outerTranslator =>

  // Requirements
  protected def translatePermission(permission : chalice.Permission) : Expression
  protected def lookupLogicalVariable(name : String) : Option[LogicalVariable]

  final def translateExpression(expression : chalice.Expression) : Expression = termTranslation.orElse[chalice.Expression,Expression]({
    case x =>
      report(messages.UnknownAstNode(x))
      dummyExpression(x)
  })(expression)

  final def dummyExpression(location : SourceLocation) = currentExpressionFactory.makeIntegerLiteralExpression(27,location,List("Dummy term used to recover from error during translation."))

  protected def termTranslation : PartialFunction[chalice.Expression,Expression] = matchingExpression {
    case rvalue@chalice.IntLiteral(i) =>
      currentExpressionFactory.makeIntegerLiteralExpression(i,rvalue)
    case rvalue@chalice.BoolLiteral(true) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Boolean.trueLiteral,ExpressionSequence(),rvalue)
    case rvalue@chalice.BoolLiteral(false) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Boolean.falseLiteral,ExpressionSequence(),rvalue)
    case rvalue@chalice.LockBottomLiteral() =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Mu().lockBottom,ExpressionSequence(),rvalue)
    case rvalue@chalice.Holds(t) => translateHolds(t,rvalue)
    case rvalue@chalice.RdHolds(t) =>
      report(messages.RdLockNotSupported(rvalue))
      // to recover, treat it like an ordinary holds
      translateHolds(t,rvalue)
    case variableExpr:chalice.VariableExpr =>
      // first look up the name as a logical (quantified) variable
      lookupLogicalVariable(variableExpr.v.UniqueName) match {
        case Some(logical) =>
          currentExpressionFactory.makeBoundVariableExpression(logical,variableExpr)
        case None =>
          // otherwise, treat as program variable
          currentExpressionFactory.makeProgramVariableExpression((programVariables(variableExpr.v)),variableExpr)
      }
    case rvalue@chalice.Old(e) => currentExpressionFactory.makeOldExpression(translateExpression(e))(rvalue)
    case access@chalice.MemberAccess(rcvr,_) if !access.isPredicate =>
      assert(access.f != null,"Chalice MemberAccess node (%s) is not linked to a field.".format(access))
      val rcvrExpression = translateExpression(rcvr)
      currentExpressionFactory.makeFieldReadExpression(rcvrExpression,fields(access.f),access)
    case ifThenElse@chalice.IfThenElse(cond,thn,els) =>
      val condExpression = translateExpression(cond)
      val thnExpression = translateExpression(thn)
      val elsExpression = translateExpression(els)
      currentExpressionFactory.makeIfThenElseExpression(condExpression,thnExpression,elsExpression)(ifThenElse)
    case th@chalice.ImplicitThisExpr() =>
      currentExpressionFactory.makeProgramVariableExpression(thisVariable,th)
    case th@chalice.ExplicitThisExpr() =>
      currentExpressionFactory.makeProgramVariableExpression(thisVariable,th)
    case rvalue@chalice.And(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Boolean.logicalAnd,ExpressionSequence(
        translateExpression(lhs),
        translateExpression(rhs)
      ),rvalue)
    case rvalue@chalice.Or(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Boolean.logicalOr,ExpressionSequence(
        translateExpression(lhs),
        translateExpression(rhs)
      ),rvalue)
    case rvalue@chalice.Implies(lhs,rhs) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Boolean.implication,ExpressionSequence(
        translateExpression(lhs),
        translateExpression(rhs)
      ),rvalue)
    case rvalue@chalice.Not(op) =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Boolean.not,ExpressionSequence(
        translateExpression(op)
      ),rvalue)
    case literal@chalice.NullLiteral() =>
      currentExpressionFactory.makeDomainFunctionApplicationExpression(nullFunction,ExpressionSequence(),literal)
    case unfolding@chalice.Unfolding(predicateAccess, body) =>
      val location = translateExpression(predicateAccess.ma.e)
      val predicateExpression = makePredicatePermissionExpression(location,
        predicates(predicateAccess.ma.predicate),
        translatePermission(predicateAccess.perm),unfolding)
      (predicateExpression,translateExpression(body)) match {
        case (pred:PredicatePermissionExpression,bodyExpression:Expression) =>
          currentExpressionFactory.makeUnfoldingExpression(pred,bodyExpression,unfolding)
        case (_,bodyExpression) =>
          currentExpressionFactory.makeUnfoldingExpression(predicateExpression,bodyExpression,unfolding)
      }
    case functionApplication@chalice.FunctionApplication(receiver,_,args) =>
      currentExpressionFactory.makeFunctionApplicationExpression(
        translateExpression(receiver),
        functions(functionApplication.f),
        ExpressionSequence(args.map(translateExpression(_)):_*),functionApplication)
    case binary:chalice.BinaryExpr => translateBinaryExpression(binary)
  }

  protected def translateHolds(obj : chalice.Expression, location : SourceLocation) : Expression = {
    val heldMap = currentExpressionFactory.makeFieldReadExpression(
      environmentCurrentThreadExpression(location),
      prelude.Thread.heldMap, location)
    currentExpressionFactory.makeDomainFunctionApplicationExpression(
      prelude.Map.HeldMap.get, ExpressionSequence(heldMap, translateExpression(obj)), location) : Expression

  }

  protected def makePredicatePermissionExpression(location : Expression, predicateFactory : PredicateFactory,permission : Expression, sourceLocation : SourceLocation ) : PredicatePermissionExpression =
    (location,permission) match {
    case (pLocation : Expression,pPermission : Expression) => currentExpressionFactory.makePredicatePermissionExpression(pLocation,predicateFactory,pPermission,sourceLocation)
    case _ => currentExpressionFactory.makePredicatePermissionExpression(location,predicateFactory,permission,sourceLocation)
  }

  protected def translateBinaryExpression(binary : chalice.BinaryExpr) : Expression = {
    val (lhs,rhs) = (binary.E0,binary.E1)
    // binary.ExpectedXhsType is often null, use the "inferred" types for the operands instead
    val (lhsType,rhsType,resultType) = (translateClassRef(lhs.typ),translateClassRef(rhs.typ),translateClassRef(binary.ResultType))

    domainFunctionLookup.lookup(List(lhsType,rhsType,resultType).map(_.domain))(binary.OpName,List(Some(lhsType),Some(rhsType))) match {
      case Success(e) => currentExpressionFactory.makeDomainFunctionApplicationExpression(e,ExpressionSequence(translateExpression(lhs),translateExpression(rhs)),binary)
      case _ if binary.OpName == "!=" => // If no NEQ operator exists, translate as ¬(_ == _)
        val eq = chalice.Eq(binary.E0,binary.E1)
        eq.typ = binary.ResultType
        eq.pos = binary.pos
        val neg = chalice.Not(eq)
        neg.typ = binary.ResultType
        neg.pos = binary.pos
        translateExpression(neg)
      case _ =>
        report(messages.OperatorNotFound(binary,lhsType,rhsType,resultType))
        dummyExpression(binary)
    }
  }
}
