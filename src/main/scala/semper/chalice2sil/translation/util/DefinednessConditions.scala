package semper.chalice2sil.translation.util

import semper.chalice2sil
import chalice2sil._
import semper.sil.ast.source.NoLocation
import semper.sil.ast.expressions.util.ExpressionSequence
import translation._
import semper.sil.ast.expressions._
import semper.sil.ast.symbols.logical.{Not, Implication, And}
import terms._
import semper.sil.ast.types._

/**
  * @author Christian Klauser
  */
class DefinednessConditions(environment : MemberEnvironment, val readFractionExpression : Expression)
  extends DerivedMemberEnvironment(environment)
  with ExpressionVisitor[Null, Expression] {

  protected def merge(left : Expression, right : Expression) = (left,right) match {
    case (TrueExpression(),rhs) => rhs
    case (lhs,TrueExpression()) => lhs
    case (lhs,rhs) =>
      currentExpressionFactory.makeBinaryExpression(And()(lhs.sourceLocation),lhs,rhs,lhs.sourceLocation)
  }

  protected def zero = TrueExpression()(NoLocation)
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
    case FieldReadExpression(loc@FieldLocation(receiver,field)) =>
      // A field access `x.f` requires `x != null ∧ 0 < perm(x.f)`
      // `0 < perm(x.f)`
      val hasReadAccess = currentExpressionFactory.makeDomainPredicateExpression(permissionLT,
        ExpressionSequence(
          currentExpressionFactory.makeNoPermission(expression.sourceLocation),
          currentExpressionFactory.makePermExpression(receiver,field)(expression.sourceLocation)
        ),expression.sourceLocation)

      // `x == null`
      val eqNull = currentExpressionFactory.makeEqualityExpression(receiver,
        currentExpressionFactory.makeDomainFunctionApplicationExpression(nullFunction,ExpressionSequence(),expression.sourceLocation),expression.sourceLocation)
      // `¬(x == null)`
      val notNull = currentExpressionFactory.makeUnaryExpression(Not()(expression.sourceLocation),eqNull,expression.sourceLocation)

      // `combinedPrecondition(x) ∧ ¬(x == null) ∧ acc(x.f,k)`
      mergeMany(Seq(visitExpression(receiver,arg),notNull,hasReadAccess))
    case FunctionApplicationExpression(receiver,function,args) =>
      // A function application `x.f(args...)` requires `x != null ∧ precondition(f,args...)`

      // `x == null`
      val eqNull = currentExpressionFactory.makeEqualityExpression(receiver,
        currentExpressionFactory.makeDomainFunctionApplicationExpression(nullFunction,ExpressionSequence(),expression.sourceLocation),expression.sourceLocation)
      // `¬(x == null)`
      val notNull = currentExpressionFactory.makeUnaryExpression(Not()(expression.sourceLocation),eqNull,expression.sourceLocation)

      // precondition(f,args..) (without side effects)
      val subs = currentExpressionFactory.makeProgramVariableSubstitution(
        function.signature.parameters.zip(args).toSet)
      val preconditions = function.signature.precondition.map(x => removeSideEffects(x.substitute(subs)))

      mergeMany(Seq(notNull) ++ preconditions)
    case DomainFunctionApplicationExpression(f,ts) if ts.size == 2 && f == booleanImplication =>
      val lhsExpression = ts(0)
      val rhsExpression = ts(1);
      // `lhsPrecondition ∧ (eval(lhs) ⇒ rhsPrecondition)`
      {
        val lhsPrecondition = visitExpression(lhsExpression,arg)
        val rhsPrecondition = visitExpression(rhsExpression,arg)
        // `eval(lhs)`
        val lhsCond = currentExpressionFactory.makeDomainPredicateExpression(booleanEvaluate,ExpressionSequence(lhsExpression),expression.sourceLocation)
        // `eval(lhs) ⇒ rhsPrecondition`
        val rhsBranch = currentExpressionFactory.makeBinaryExpression(Implication()(expression.sourceLocation),lhsCond,rhsPrecondition,expression.sourceLocation)
        // `lhsPrecondition ∧ (eval(lhs) ⇒ rhsPrecondition)`
        merge(lhsPrecondition,rhsBranch)
      }
    case IfThenElseExpression(cond,thn,els) =>
      val condPrecondition = visitExpression(cond,arg)
      val condExpr = currentExpressionFactory.makeDomainPredicateExpression(booleanEvaluate,ExpressionSequence(cond),expression.sourceLocation)
      val thnPrecondition = visitExpression(thn,arg)
      val elsPrecondition = visitExpression(els,arg);
      // `condPrecondition ∧ (cond ⇒ thnPrecondition) ∧ (¬cond ⇒ elsPrecondition)`
      {
        // `cond ⇒ thnPrecondition`
        val thnBranch = currentExpressionFactory.makeBinaryExpression(Implication()(expression.sourceLocation),condExpr,thnPrecondition,expression.sourceLocation)
        // `¬cond`
        val notCondExpr = currentExpressionFactory.makeUnaryExpression(Not()(expression.sourceLocation),condExpr,expression.sourceLocation)
        // `¬cond ⇒ elsPrecondition`
        val elsBranch = currentExpressionFactory.makeBinaryExpression(Implication()(expression.sourceLocation),notCondExpr,elsPrecondition,expression.sourceLocation)
        // finally: `condPrecondition ∧ (cond ⇒ thnPrecondition) ∧ (¬cond ⇒ elsPrecondition)`
        mergeMany(Seq(condPrecondition, thnBranch, elsBranch))
      }
    case _ => super.visitExpression(expression,arg)
  }
}
