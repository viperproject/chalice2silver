package semper.chalice2sil.translation

import scala.language.reflectiveCalls
import operators.Lookup._
import semper.sil.ast.expressions.{TrueExpression, FalseExpression, Expression}
import semper.sil.ast.expressions.util.ExpressionSequence
import semper.sil.ast.symbols.logical.{Not, And, Or, Implication}
import semper.sil.ast.symbols.logical.quantification.{LogicalVariable, Quantifier, Forall, Exists}
import semper.sil.ast.types.{referenceType, permissionLT, permissionType}
import semper.sil.ast.expressions.terms.FullPermissionExpression
import semper.sil.ast.source.SourceLocation
import semper.chalice2sil.translation.util.PureLanguageConstruct
import semper.chalice2sil._
import collection.mutable

/**
 * @author Christian Klauser
 */
trait ExpressionTranslator extends MemberEnvironment with TypeTranslator {
  outerTranslator =>
  protected def translateExpression(exprNode: chalice.Expression): Expression

  protected def translatePermission(permission: chalice.Permission): Expression

  protected lazy val expressionTranslationHandler = expressionTranslation orElse fallbackToExpressions orElse missingTranslation

  final def withWaitlevel(waitlevel: SourceLocation, quantifier: Option[Quantifier] = None)(f: Function[Expression, Expression]) = {
    // ∀ o:ref :: $CurrentThread.heldMap[o] ⇒ f($CurrentThread.muMap[o])
    val heldMap = currentExpressionFactory.makeFieldReadExpression(environmentCurrentThreadExpression(waitlevel), prelude.Thread.heldMap, waitlevel)
    val oVar = currentExpressionFactory.makeBoundVariable(getNextName("o"), referenceType, waitlevel)
    val oVarExpression = currentExpressionFactory.makeBoundVariableExpression(oVar, waitlevel)
    val isHeld = currentExpressionFactory.makeDomainPredicateExpression(prelude.Boolean.eval, ExpressionSequence(
      currentExpressionFactory.makeDomainFunctionApplicationExpression(
        prelude.Map.HeldMap.get, ExpressionSequence(heldMap, oVarExpression), waitlevel)
    ), waitlevel)
    val muMap = currentExpressionFactory.makeFieldReadExpression(environmentCurrentThreadExpression(waitlevel), prelude.Thread.muMap, waitlevel)
    val oMu = currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Map.MuMap.get, ExpressionSequence(muMap, oVarExpression), waitlevel)
    val impl = currentExpressionFactory.makeBinaryExpression(Implication()(waitlevel), isHeld, f(oMu), waitlevel)
    currentExpressionFactory.makeQuantifierExpression(quantifier.getOrElse(Forall()(waitlevel)), oVar, impl)(waitlevel)
  }

  protected def eqWaitlevel(sameAsWaitlevel: SourceLocation, waitlevel: SourceLocation, other: chalice.Expression): Expression = {
    // other == max {o | o:ref, $CurrentThread.heldMap[o]}
    //  <==>
    // (∃ o:ref :: $CurrentThread.heldMap[o] ⇒ other == $CurrentThread.muMap[o])
    //  && (∀ o:ref :: $CurrentThread.heldMap[o] ⇒ !(other < $CurrentThread.muMap[o]))
    val maxIsHeld = withWaitlevel(sameAsWaitlevel, Some(Exists()(sameAsWaitlevel))) {
      oMu =>
        currentExpressionFactory.makeEqualityExpression(oMu, translateExpression(other), sameAsWaitlevel)
    }
    val isSupremum = withWaitlevel(sameAsWaitlevel) {
      oMu =>
        currentExpressionFactory.makeUnaryExpression(Not()(sameAsWaitlevel)
          , currentExpressionFactory.makeDomainPredicateExpression(prelude.Mu().below, ExpressionSequence(
            translateExpression(other), oMu
          ), sameAsWaitlevel)
          , sameAsWaitlevel)
    }

    currentExpressionFactory.makeBinaryExpression(And()(sameAsWaitlevel), maxIsHeld, isSupremum, sameAsWaitlevel)
  }

  protected def expressionTranslation: PartialFunction[chalice.Expression, Expression] = matchingExpression {
    case expression@chalice.Old(inner) =>
      currentExpressionFactory.makeOldExpression(translateExpression(inner))(expression)
    case sameAsWaitlevel@chalice.Eq(chalice.MaxLockLiteral(), chalice.MaxLockLiteral()) =>
      TrueExpression()(sameAsWaitlevel, List("waitlevel == waitlevel"))
    case sameAsWaitlevel@chalice.Eq(waitlevel@chalice.MaxLockLiteral(), other) =>
      eqWaitlevel(sameAsWaitlevel, waitlevel, other)
    case sameAsWaitlevel@chalice.Eq(other, waitlevel@chalice.MaxLockLiteral()) =>
      eqWaitlevel(sameAsWaitlevel, waitlevel, other)
    case expression@chalice.And(lhs, rhs) =>
      val lhsT = translateExpression(lhs)
      val rhsT = translateExpression(rhs)
      currentExpressionFactory.makeBinaryExpression(And()(expression), lhsT, rhsT, expression)
    case expression@chalice.Or(lhs, rhs) =>
      val lhsT = translateExpression(lhs)
      val rhsT = translateExpression(rhs)
      currentExpressionFactory.makeBinaryExpression(Or()(expression), lhsT, rhsT, expression)
    case expression@chalice.Implies(lhs, rhs) =>
      val lhsT = translateExpression(lhs)
      val rhsT = translateExpression(rhs)
      currentExpressionFactory.makeBinaryExpression(Implication()(expression), lhsT, rhsT, expression)
    case equality@chalice.Eq(lhs, rhs) =>
      val lhsExpression = translateExpression(lhs)
      val rhsExpression = translateExpression(rhs)
      currentExpressionFactory.makeEqualityExpression(lhsExpression, rhsExpression, equality)
    case inequality@chalice.Neq(lhs, rhs) =>
      val lhsExpression = translateExpression(lhs)
      val rhsExpression = translateExpression(rhs)
      val eq = currentExpressionFactory.makeEqualityExpression(lhsExpression, rhsExpression, inequality)
      currentExpressionFactory.makeUnaryExpression(Not()(inequality), eq, inequality)
    case ifThenElse@chalice.IfThenElse(cond, thn, els) =>
      val condExpr = translateExpression(cond)
      val thnExpr = translateExpression(thn)
      val elsExpr = translateExpression(els)
      val pos = currentExpressionFactory.makeBinaryExpression(Implication()(ifThenElse), condExpr, thnExpr, ifThenElse)
      val negCondExpr = currentExpressionFactory.makeUnaryExpression(Not()(ifThenElse), condExpr, ifThenElse)
      val neg = currentExpressionFactory.makeBinaryExpression(Implication()(ifThenElse), negCondExpr, elsExpr, ifThenElse)
      currentExpressionFactory.makeBinaryExpression(And()(ifThenElse), pos, neg, ifThenElse)
    case f@chalice.LockBelow(chalice.MaxLockLiteral(), chalice.MaxLockLiteral()) =>
      FalseExpression()(f, List("waitlevel << waitlevel")): Expression
    case aboveWaitlevel@chalice.LockBelow(waitlevel@chalice.MaxLockLiteral(), other) =>
      // ∀ o:ref :: $CurrentThread.heldMap[o] ⇒ $CurrentThread.muMap[o] << other
      withWaitlevel(waitlevel) {
        oMu =>
          currentExpressionFactory.makeDomainPredicateExpression(prelude.Mu().below, ExpressionSequence(oMu, translateExpression(other)), aboveWaitlevel)
      }
    case belowWaitlevel@chalice.LockBelow(other, waitlevel@chalice.MaxLockLiteral()) =>
      // ∀ o:ref :: $CurrentThread.heldMap[o] ⇒ other << $CurrentThread.muMap[o]
      withWaitlevel(waitlevel) {
        oMu =>
          currentExpressionFactory.makeDomainPredicateExpression(prelude.Mu().below, ExpressionSequence(translateExpression(other), oMu), belowWaitlevel)
      }
    case binary: chalice.BinaryExpr =>
      val (lhs, rhs) = (binary.E0, binary.E1)
      // binary.ExpectedXhsType is often null, use the "inferred" types for the operands instead
      val (lhsType, rhsType, resultType) = (translateClassRef(lhs.typ), translateClassRef(rhs.typ), translateClassRef(binary.ResultType))

      domainPredicateLookup.lookup(List(lhsType, rhsType, resultType).map(_.domain))(binary.OpName, List(Some(lhsType), Some(rhsType))) match {
        case Success(e) =>
          currentExpressionFactory.makeDomainPredicateExpression(e, ExpressionSequence(translateExpression(lhs), translateExpression(rhs)), binary)
        case Ambiguous(ops) =>
          report(messages.OperatorNotFound(binary, lhsType, rhsType, resultType))
          dummyExpr(currentExpressionFactory, binary)
        case Failure() =>
          //Fall back to translating a boolean term and use Boolean.Eval
          val term = translateExpression(binary)
          if (term.dataType.isCompatible(prelude.Boolean.dataType)) {
            currentExpressionFactory.makeDomainPredicateExpression(prelude.Boolean.eval, ExpressionSequence(term), binary)
          } else {
            report(messages.ExpressionInExpressionPosition(binary, term.dataType))
            dummyExpr(currentExpressionFactory, binary)
          }
      }
    case expression@chalice.Access(chalice.MemberAccess(tokenExpr, joinableName), permission)
      if tokenExpr.typ.IsToken && joinableName == prelude.Token.joinable.name => {
      // translate `acc(token.joinable,X)` to `(acc(token.joinable,X) && acc(token.args/olds,X)...)`
      // i.e., make sure that arg/old fields always have the same permissions as token.joinable.
      val m = methods(tokenExpr.typ.asInstanceOf[chalice.TokenClass].method)
      val tokenExpression = translateExpression(tokenExpr)
      translateAccessExpression(permission) {
        permAmount =>
          m.callToken.allFields
            .map(currentExpressionFactory.makeFieldPermissionExpression(tokenExpression, _, permAmount, expression))
            .reduce[Expression](currentExpressionFactory.makeBinaryExpression(And()(expression), _, _, expression))
      }
    }
    case expression@chalice.Access(fieldAccess@chalice.MemberAccess(objectReference, muName), permission)
      if fieldAccess.typ.IsMu && muName == prelude.Object.mu.name => {
      // translate `acc(x.mu,p) to `acc(x.mu,p) && $CurrentThread.muMap[x] == x.mu`
      //  linking the muMap to the actual value of mu
      val muMap = currentExpressionFactory.makeFieldReadExpression(environmentCurrentThreadExpression(fieldAccess), prelude.Thread.muMap, fieldAccess)
      val objRefExpression = translateExpression(objectReference)
      val link = currentExpressionFactory.makeEqualityExpression(
        currentExpressionFactory.makeDomainFunctionApplicationExpression(prelude.Map.MuMap.get, ExpressionSequence(muMap, objRefExpression), fieldAccess),
        currentExpressionFactory.makeFieldReadExpression(objRefExpression, prelude.Object.mu, fieldAccess), expression
      )
      currentExpressionFactory.makeBinaryExpression(And()(expression),
        translateAccessExpression(permission)(currentExpressionFactory.makeFieldPermissionExpression(objRefExpression, prelude.Object.mu, _, expression)),
        link, expression
      )
    }: Expression
    case ma@chalice.MemberAccess(target, _) if ma.isPredicate =>
      currentExpressionFactory.makePredicatePermissionExpression(translateExpression(target), predicates(ma.predicate), FullPermissionExpression()(ma, Nil), ma)
    case expression@chalice.Access(memberAccess, permission) if !memberAccess.isPredicate =>
      translateAccessExpression(permission)(
        currentExpressionFactory.makeFieldPermissionExpression(translateExpression(memberAccess.e), fields(memberAccess.f), _, expression))
    case expression@chalice.Access(predicateAccess, permission) if predicateAccess.isPredicate =>
      translateAccessExpression(permission)(currentExpressionFactory.makePredicatePermissionExpression(
        translateExpression(predicateAccess.e),
        predicates(predicateAccess.predicate),
        _,
        expression))
    case unfolding@chalice.Unfolding(predicateAccess, body) =>
      val location = translateExpression(predicateAccess.ma.e)
      val permissionExpr = currentExpressionFactory.makePredicatePermissionExpression(location,
        predicates(predicateAccess.ma.predicate),
        translatePermission(predicateAccess.perm), unfolding)
      currentExpressionFactory.makeUnfoldingExpression(
        permissionExpr, translateExpression(body), unfolding)
    case eval@chalice.Eval(forkState@chalice.CallState(token, receiver, _, args), chalice.BoolLiteral(true)) =>
      // just associate the receiver and args with the corresponding fields on the token
      val mf = methods(forkState.m)
      // create pairs  (SIL term, token field)
      val pairs = ((receiver :: args).zip(mf.callToken.args)).map(pair =>
        (translateExpression(pair._1), pair._2))
      // create equations (SIL term == tokenExpression.tokenField)
      val tokenExpression = translateExpression(token)
      val eqns = pairs.map(pair =>
        currentExpressionFactory.makeEqualityExpression(pair._1, currentExpressionFactory.makeFieldReadExpression(tokenExpression, pair._2.field, token), eval): Expression)
      // connect equations with && (we can safely use reduce, since the receiver == token.receiver will always be included)
      eqns.reduce(currentExpressionFactory.makeBinaryExpression(And()(eval), _, _, eval))
    case eval: chalice.Eval =>
      report(messages.GeneralEvalNotImplemented(eval))
      dummyExpr(currentExpressionFactory, eval)
    case quantification@chalice.TypeQuantification(q, _, _, e, null) =>
      val quantifier = q match {
        case chalice.Exists => Exists()(quantification)
        case chalice.Forall => Forall()(quantification)
      }
      // recursively traverse the list of quantified variables.
      //  - on descent, create the logical variables and add them to a map
      //  - at the end, use that map to translate the quantifier body
      //  - on ascent, wrap the expression from the previous level in a quantifier expression
      def applyQuantifier(variablesLeft: List[chalice.Variable], boundVariables: Map[String, LogicalVariable]): Expression = variablesLeft match {
        case v :: vs =>
          val t = translateTypeExpr(v.t)
          val bv = currentExpressionFactory.makeBoundVariable(v.UniqueName, t, v)
          currentExpressionFactory.makeQuantifierExpression(
            quantifier, bv, applyQuantifier(vs, boundVariables + (bv.name -> bv)))(quantification)
        case Nil =>
          withScope(boundVariables) {
            translateExpression(e)
          }
      }
      applyQuantifier(quantification.variables, Map.empty)
    case node: chalice.Quantification =>
      report(messages.SequenceQuantificationNotImplemented(node))
      dummyExpr(currentExpressionFactory, node)
  }

  protected def translateAccessExpression(permission: chalice.Permission)(body: Expression => Expression): Expression = permission match {
    case s@chalice.Star =>
      val ctor = new PureLanguageConstruct(this, s)
      import ctor._
      val readFractionVar = currentExpressionFactory.makeBoundVariable(getNextName("kstar"), permissionType, s)
      val readFractionExpression = currentExpressionFactory.makeBoundVariableExpression(readFractionVar, s)
      currentExpressionFactory.makeQuantifierExpression(Exists()(s), readFractionVar, conjunction(
        permissionLT.apply(noPermission, readFractionExpression), // 0 < k
        permissionLT.apply(readFractionExpression, fullPermission), // k < write
        permissionLT.apply(readFractionExpression, environmentReadFractionExpression(permission)), // k < k_method
        body(readFractionExpression)))(s) // body(k)
    case amount => body(translatePermission(amount))
  }

  protected def fallbackToExpressions = matchingExpression {
    case boolExpr if boolExpr.typ == chalice.BoolClass =>
      val boolExpression = translateExpression(boolExpr)
      currentExpressionFactory.makeDomainPredicateExpression(prelude.Boolean.eval, ExpressionSequence(boolExpression), boolExpr)
    // Warning: this code is duplicated in quantifierBodyTranslator below
  }

  protected def missingTranslation[E] = new PartialFunction[chalice.Expression, E] {
    def isDefinedAt(e: chalice.Expression) = false

    def apply(expression: chalice.Expression) = {
      report(messages.UnknownAstNode(expression))
      dummyExpr(currentExpressionFactory, expression).asInstanceOf[E]
    }
  }

  protected def quantifierScopes: mutable.Stack[Map[String, LogicalVariable]]

  final def lookupLogicalVariable(name: String): Option[LogicalVariable] =
    quantifierScopes.map(_.get(name)).collectFirst({
      case Some(x) => x
    })

  final def withScope[T](scope: Map[String, LogicalVariable])(body: => T): T = {
    quantifierScopes.push(scope)
    try {
      body
    } finally {
      quantifierScopes.pop()
    }
  }
}

