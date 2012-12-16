package semper.chalice2sil.translation.util

import semper.chalice2sil
import chalice2sil._
import semper.sil.ast.source.{SourceLocation, noLocation}
import semper.sil.ast.expressions.util.TermSequence
import translation._
import semper.sil.ast.expressions._
import semper.sil.ast.symbols.logical.{Not, Implication, And}
import terms._
import semper.sil.ast.types._

/**
  * @author Christian Klauser
  */
class DefinednessConditions(environment : MemberEnvironment, val readFractionTerm : Term)
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
        merge(lhsPrecondition,rhsBranch)
      }
    case _ => super.visitExpression(expression,arg)
  }

  override def visitTerm(term : Term, arg : Null) : Expression = term match {
    case FieldReadTerm(loc@FieldLocation(receiver,field)) =>
      // A field access `x.f` requires `x != null ∧ 0 < perm(x.f)`
      // `0 < perm(x.f)`
      val hasReadAccess = currentExpressionFactory.makeDomainPredicateExpression(permissionLT,
        TermSequence(
          currentExpressionFactory.makeNoPermission(term.sourceLocation),
          currentExpressionFactory.makePermTerm(receiver,field)(term.sourceLocation)
        ),term.sourceLocation)

      // `x == null`
      val eqNull = currentExpressionFactory.makeEqualityExpression(receiver,
        currentExpressionFactory.makeDomainFunctionApplicationTerm(nullFunction,TermSequence(),term.sourceLocation),term.sourceLocation)
      // `¬(x == null)`
      val notNull = currentExpressionFactory.makeUnaryExpression(Not()(term.sourceLocation),eqNull,term.sourceLocation)

      // `combinedPrecondition(x) ∧ ¬(x == null) ∧ acc(x.f,k)`
      mergeMany(Seq(visitTerm(receiver,arg),notNull,hasReadAccess))
    case FunctionApplicationTerm(receiver,function,args) =>
      // A function application `x.f(args...)` requires `x != null ∧ precondition(f,args...)`

      // `x == null`
      val eqNull = currentExpressionFactory.makeEqualityExpression(receiver,
        currentExpressionFactory.makeDomainFunctionApplicationTerm(nullFunction,TermSequence(),term.sourceLocation),term.sourceLocation)
      // `¬(x == null)`
      val notNull = currentExpressionFactory.makeUnaryExpression(Not()(term.sourceLocation),eqNull,term.sourceLocation)

      // precondition(f,args..) (without side effects)
      val subs = currentExpressionFactory.makeProgramVariableSubstitution(
        function.signature.parameters.zip(args).toSet)
      val preconditions = function.signature.precondition.map(x => removeSideEffects(x.substitute(subs)))

      mergeMany(Seq(notNull) ++ preconditions)
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
        merge(lhsPrecondition,rhsBranch)
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
        // finally: `condPrecondition ∧ (cond ⇒ thnPrecondition) ∧ (¬cond ⇒ elsPrecondition)`
        mergeMany(Seq(condPrecondition, thnBranch, elsBranch))
      }
    case _ => super.visitTerm(term,arg)
  }
}
