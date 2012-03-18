package ch.ethz.inf.pm.semper.chalice2sil.translation

import operators.Lookup._
import silAST.symbols.logical.And._
import silAST.symbols.logical.Or._
import silAST.symbols.logical.Implication._
import silAST.domains.{DomainPredicate, Domain}
import silAST.types.DataType
import silAST.expressions.util.TermSequence._
import silAST.expressions.{ExpressionFactory, Expression}
import silAST.expressions.util.TermSequence
import silAST.expressions.terms.Term
import silAST.ASTNode
import ch.ethz.inf.pm.semper.chalice2sil._
import silAST.symbols.logical.{Not, And, Or, Implication}

/**
  * @author Christian Klauser
  */
trait ExpressionTranslator extends MethodEnvironment {
  protected def translateTerm(exprNode :  chalice.Expression ) : Term
  protected def translateClassRef(classRef : chalice.Class) : DataType
  protected def translatePermission(permission : chalice.Permission) : Term
  
  protected final def matchingExpression(partialFunction : PartialFunction[chalice.Expression, Expression]) : PartialFunction[chalice.Expression, Expression] = partialFunction

  def translateExpression(e : chalice.Expression) : Expression = expressionTranslation(e)

  protected def expressionTranslation : PartialFunction[chalice.Expression, Expression] = matchingExpression {
      case expression@chalice.Old(inner) =>
        currentExpressionFactory.makeOldExpression(expression,translateExpression(inner))
      case expression@chalice.And(lhs,rhs) =>
        val lhsT = translateExpression(lhs)
        val rhsT = translateExpression(rhs)
        currentExpressionFactory.makeBinaryExpression(expression,And()(expression),lhsT,rhsT)
      case expression@chalice.Or(lhs,rhs) =>
        val lhsT = translateExpression(lhs)
        val rhsT = translateExpression(rhs)
        currentExpressionFactory.makeBinaryExpression(expression,Or()(expression),lhsT,rhsT)
      case expression@chalice.Implies(lhs,rhs) =>
        val lhsT = translateExpression(lhs)
        val rhsT = translateExpression(rhs)
        currentExpressionFactory.makeBinaryExpression(expression,Implication()(expression),lhsT,rhsT)
      case equality@chalice.Eq(lhs,rhs) => 
        val lhsTerm = translateTerm(lhs)
        val rhsTerm = translateTerm(rhs)
        currentExpressionFactory.makeEqualityExpression(equality,lhsTerm,rhsTerm)
      case inequality@chalice.Neq(lhs,rhs) =>
        val lhsTerm = translateTerm(lhs)
        val rhsTerm = translateTerm(rhs)
        val eq = currentExpressionFactory.makeEqualityExpression(inequality,lhsTerm,rhsTerm)
        currentExpressionFactory.makeUnaryExpression(inequality,Not()(inequality),eq)
      case ifThenElse@chalice.IfThenElse(cond,thn,els) =>
        val condExpr = translateExpression(cond)
        val thnExpr = translateExpression(thn)
        val elsExpr = translateExpression(els)
        val pos = currentExpressionFactory.makeBinaryExpression(ifThenElse,Implication()(ifThenElse),condExpr,thnExpr)
        val negCondExpr = currentExpressionFactory.makeUnaryExpression(ifThenElse,Not()(ifThenElse),condExpr)
        val neg = currentExpressionFactory.makeBinaryExpression(ifThenElse,Implication()(ifThenElse),negCondExpr,elsExpr)
        currentExpressionFactory.makeBinaryExpression(ifThenElse,And()(ifThenElse),pos,neg)
      case binary:chalice.BinaryExpr =>
        val (lhs,rhs) = (binary.E0,binary.E1)
        // binary.ExpectedXhsType is often null, use the "inferred" types for the operands instead
        val (lhsType,rhsType,resultType) = (translateClassRef(lhs.typ),translateClassRef(rhs.typ),translateClassRef(binary.ResultType))

        domainPredicateLookup.lookup(List(lhsType,rhsType,resultType).map(_.domain))(binary.OpName,List(Some(lhsType),Some(rhsType))) match {
          case Success(e) =>
            currentExpressionFactory.makeDomainPredicateExpression(binary,e,TermSequence(translateTerm(lhs),translateTerm(rhs)))
          case Ambiguous(ops) =>
            report(messages.OperatorNotFound(binary,lhsType,rhsType,resultType))
            dummyExpr(currentExpressionFactory,binary)
          case Failure() =>
            //Fall back to translating a boolean term and use Boolean.Eval
            val term = translateTerm(binary)
            if(term.dataType.isCompatible(prelude.Boolean.dataType)) {
              currentExpressionFactory.makeDomainPredicateExpression(binary,prelude.Boolean.eval,TermSequence(term))
            } else {
              report(messages.TermInExpressionPosition(binary,term.dataType))
              dummyExpr(currentExpressionFactory,binary)
            }
        }
      case expression@chalice.Access(memberAccess, permission) =>
        currentExpressionFactory.makePermissionExpression(expression,translateTerm(memberAccess.e),fields(memberAccess.f),translatePermission(permission))
      case ma@chalice.MemberAccess(target,_) if ma.isPredicate =>
        report(messages.UnknownAstNode(ma))
        dummyExpr(currentExpressionFactory,ma)
      case boolExpr if boolExpr.typ == chalice.BoolClass =>
        val boolTerm = translateTerm(boolExpr)
        currentExpressionFactory.makeDomainPredicateExpression(boolExpr,prelude.Boolean.eval,TermSequence(boolTerm))

  } orElse missingTranslation

  protected def missingTranslation[E] = new PartialFunction[chalice.Expression,E] {
    def isDefinedAt(e : chalice.Expression) = false
    def apply(expression : chalice.Expression) = {
      report(messages.UnknownAstNode(expression))
      dummyExpr(currentExpressionFactory,expression).asInstanceOf[E]
    }
  }
}

