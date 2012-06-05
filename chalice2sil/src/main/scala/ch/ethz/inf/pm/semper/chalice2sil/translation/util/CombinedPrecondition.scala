package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import ch.ethz.inf.pm.semper.chalice2sil._
import silAST.source.noLocation
import silAST.expressions.util.TermSequence
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
      currentExpressionFactory.makeBinaryExpression(And()(lhs.sourceLocation),lhs,rhs,lhs.sourceLocation)
  }

  protected def zero = TrueExpression()(noLocation)
  override def visitExpression(expression : Expression, arg : Null) : Expression = expression match {
    case BinaryExpression(Implication(),lhs,rhs) =>
      // `lhsPrecondition ∧ (lhs ⇒ rhsPrecondition)`
      {
        val lhsPrecondition = visitExpression(lhs,arg)
        val rhsPrecondition = visitExpression(rhs,arg)
        // `lhs ⇒ rhsPrecondition`
        val rhsBranch = currentExpressionFactory.makeBinaryExpression(Implication()(expression.sourceLocation),lhs,rhsPrecondition,expression.sourceLocation)
        // `lhsPrecondition ∧ (lhs ⇒ rhsPrecondition)`
        currentExpressionFactory.makeBinaryExpression(And()(expression.sourceLocation),lhsPrecondition,rhsBranch,expression.sourceLocation)
      }
    case _ => super.visitExpression(expression,arg)
  }

  override def visitTerm(term : Term, arg : Null) : Expression = term match {
    case FieldReadTerm(FieldLocation(receiver,field)) =>
      // A field access `x.f` requires `x != null ∧ acc(x.f,k)`, k is the current read permission fraction
      // `acc(x.f,k)`
      val hasReadAccess = currentExpressionFactory.makeFieldPermissionExpression(receiver,field,readFractionTerm,term.sourceLocation)
      // `x == null`
      val eqNull = currentExpressionFactory.makeEqualityExpression(receiver,
        currentExpressionFactory.makeDomainFunctionApplicationTerm(nullFunction,TermSequence(),term.sourceLocation),term.sourceLocation)
      // `¬(x == null)`
      val notNull = currentExpressionFactory.makeUnaryExpression(Not()(term.sourceLocation),eqNull,term.sourceLocation)
      // `¬(x == null) ∧ acc(x.f,k)`
      currentExpressionFactory.makeBinaryExpression(And()(term.sourceLocation),notNull,hasReadAccess,term.sourceLocation)
    case DomainFunctionApplicationTerm(f,ts) if ts.size == 2 && f == booleanImplication =>
      val lhsTerm = ts(0)
      val rhsTerm = ts(1);
      // `lhsPrecondition ∧ (eval(lhs) ⇒ rhsPrecondition)`
      {
        val lhsPrecondition = visitTerm(lhsTerm,arg)
        val rhsPrecondition = visitTerm(rhsTerm,arg)
        // `eval(lhs)`
        val lhsCond = currentExpressionFactory.makeDomainPredicateExpression(booleanEvaluate,TermSequence(lhsTerm),term.sourceLocation)
        // `eval(lhs) ⇒ rhsPrecondition`
        val rhsBranch = currentExpressionFactory.makeBinaryExpression(Implication()(term.sourceLocation),lhsCond,rhsPrecondition,term.sourceLocation)
        // `lhsPrecondition ∧ (eval(lhs) ⇒ rhsPrecondition)`
        currentExpressionFactory.makeBinaryExpression(And()(term.sourceLocation),lhsPrecondition,rhsBranch,term.sourceLocation)
      }
    case IfThenElseTerm(cond,thn,els) =>
      val condPrecondition = visitTerm(cond,arg)
      val condExpr = currentExpressionFactory.makeDomainPredicateExpression(booleanEvaluate,TermSequence(cond),term.sourceLocation)
      val thnPrecondition = visitTerm(thn,arg)
      val elsPrecondition = visitTerm(els,arg);
      // `condPrecondition ∧ (cond ⇒ thnPrecondition) ∧ (¬cond ⇒ elsPrecondition)`
      {
        // `cond ⇒ thnPrecondition`
        val thnBranch = currentExpressionFactory.makeBinaryExpression(Implication()(term.sourceLocation),condExpr,thnPrecondition,term.sourceLocation)
        // `¬cond`
        val notCondExpr = currentExpressionFactory.makeUnaryExpression(Not()(term.sourceLocation),condExpr,term.sourceLocation)
        // `¬cond ⇒ elsPrecondition`
        val elsBranch = currentExpressionFactory.makeBinaryExpression(Implication()(term.sourceLocation),notCondExpr,elsPrecondition,term.sourceLocation)
        // `(cond ⇒ thnPrecondition) ∧ (¬cond ⇒ elsPrecondition)`
        val branches = currentExpressionFactory.makeBinaryExpression(And()(term.sourceLocation),thnBranch,elsBranch,term.sourceLocation)
        // finally: `condPrecondition ∧ (cond ⇒ thnPrecondition) ∧ (¬cond ⇒ elsPrecondition)`
        currentExpressionFactory.makeBinaryExpression(And()(term.sourceLocation),condPrecondition,branches,term.sourceLocation)
      }
    case _ => super.visitTerm(term,arg)
  }
}
