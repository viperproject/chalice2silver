package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import ch.ethz.inf.pm.semper.chalice2sil._
import silAST.source.noLocation
import silAST.expressions.util.TermSequence
import collection.immutable.Map
import translation._
import silAST.expressions._
import silAST.symbols.logical.{Not, Implication, And}
import terms._
import silAST.types._

/**
  * @author Christian Klauser
  */
class CombinedPrecondition(environment : MemberEnvironment, val readFractionTerm : Term)
  extends DerivedMemberEnvironment(environment)
  with ExpressionVisitor[Null, Expression] {

  protected def merge(left : Expression, right : Expression) = (left,right) match {
    case (TrueExpression(),rhs) => rhs
    case (lhs,TrueExpression()) => lhs
    case (lhs,rhs) =>
      currentExpressionFactory.makeBinaryExpression(lhs.sourceLocation,And()(lhs.sourceLocation),lhs,rhs)
  }

  protected def zero = TrueExpression()(noLocation)
  override def visitExpression(expression : Expression, arg : Null) : Expression = expression match {
    case BinaryExpression(Implication(),lhs,rhs) =>
      // `lhsPrecondition ∧ (lhs ⇒ rhsPrecondition)`
      {
        val lhsPrecondition = visitExpression(lhs,arg)
        val rhsPrecondition = visitExpression(rhs,arg)
        // `lhs ⇒ rhsPrecondition`
        val rhsBranch = currentExpressionFactory.makeBinaryExpression(expression.sourceLocation,Implication()(expression.sourceLocation),lhs,rhsPrecondition)
        // `lhsPrecondition ∧ (lhs ⇒ rhsPrecondition)`
        currentExpressionFactory.makeBinaryExpression(expression.sourceLocation,And()(expression.sourceLocation),lhsPrecondition,rhsBranch)
      }
    case _ => super.visitExpression(expression,arg)
  }

  override def visitTerm(term : Term, arg : Null) : Expression = term match {
    case FieldReadTerm(location,field) =>
      // A field access `x.f` requires `x != null ∧ acc(x.f,k)`, k is the current read permission fraction
      // `acc(x.f,k)`
      val hasReadAccess = currentExpressionFactory.makePermissionExpression(term.sourceLocation,location,field,readFractionTerm)
      // `x == null`
      val eqNull = currentExpressionFactory.makeEqualityExpression(term.sourceLocation,location,
        currentExpressionFactory.makeDomainFunctionApplicationTerm(term.sourceLocation,nullFunction,TermSequence()))
      // `¬(x == null)`
      val notNull = currentExpressionFactory.makeUnaryExpression(term.sourceLocation,Not()(term.sourceLocation),eqNull)
      // `¬(x == null) ∧ acc(x.f,k)`
      currentExpressionFactory.makeBinaryExpression(term.sourceLocation,And()(term.sourceLocation),notNull,hasReadAccess)
    case DomainFunctionApplicationTerm(f,ts) if ts.size == 2 && f == booleanImplication =>
      val lhsTerm = ts(0)
      val rhsTerm = ts(1);
      // `lhsPrecondition ∧ (eval(lhs) ⇒ rhsPrecondition)`
      {
        val lhsPrecondition = visitTerm(lhsTerm,arg)
        val rhsPrecondition = visitTerm(rhsTerm,arg)
        // `eval(lhs)`
        val lhsCond = currentExpressionFactory.makeDomainPredicateExpression(term.sourceLocation,booleanEvaluate,TermSequence(lhsTerm))
        // `eval(lhs) ⇒ rhsPrecondition`
        val rhsBranch = currentExpressionFactory.makeBinaryExpression(term.sourceLocation,Implication()(term.sourceLocation),lhsCond,rhsPrecondition)
        // `lhsPrecondition ∧ (eval(lhs) ⇒ rhsPrecondition)`
        currentExpressionFactory.makeBinaryExpression(term.sourceLocation,And()(term.sourceLocation),lhsPrecondition,rhsBranch)
      }
    case IfThenElseTerm(cond,thn,els) =>
      val condPrecondition = visitTerm(cond,arg)
      val condExpr = currentExpressionFactory.makeDomainPredicateExpression(term.sourceLocation,booleanEvaluate,TermSequence(cond))
      val thnPrecondition = visitTerm(thn,arg)
      val elsPrecondition = visitTerm(els,arg);
      // `condPrecondition ∧ (cond ⇒ thnPrecondition) ∧ (¬cond ⇒ elsPrecondition)`
      {
        // `cond ⇒ thnPrecondition`
        val thnBranch = currentExpressionFactory.makeBinaryExpression(term.sourceLocation,Implication()(term.sourceLocation),condExpr,thnPrecondition)
        // `¬cond`
        val notCondExpr = currentExpressionFactory.makeUnaryExpression(term.sourceLocation,Not()(term.sourceLocation),condExpr)
        // `¬cond ⇒ elsPrecondition`
        val elsBranch = currentExpressionFactory.makeBinaryExpression(term.sourceLocation,Implication()(term.sourceLocation),notCondExpr,elsPrecondition)
        // `(cond ⇒ thnPrecondition) ∧ (¬cond ⇒ elsPrecondition)`
        val branches = currentExpressionFactory.makeBinaryExpression(term.sourceLocation,And()(term.sourceLocation),thnBranch,elsBranch)
        // finally: `condPrecondition ∧ (cond ⇒ thnPrecondition) ∧ (¬cond ⇒ elsPrecondition)`
        currentExpressionFactory.makeBinaryExpression(term.sourceLocation,And()(term.sourceLocation),condPrecondition,branches)
      }
    case _ => super.visitTerm(term,arg)
  }
}
