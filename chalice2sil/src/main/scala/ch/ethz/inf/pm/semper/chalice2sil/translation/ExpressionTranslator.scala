package ch.ethz.inf.pm.semper.chalice2sil.translation

import operators.Lookup._
import silAST.expressions.{TrueExpression, FalseExpression, Expression}
import silAST.expressions.util.TermSequence
import silAST.symbols.logical.{Not, And, Or, Implication}
import ch.ethz.inf.pm.semper.chalice2sil.translation.util.PureLanguageConstruct
import silAST.symbols.logical.quantification.{Forall, Exists}
import silAST.types.{referenceType, permissionLT, permissionType, DataType}
import ch.ethz.inf.pm.semper.chalice2sil._
import silAST.expressions.terms.{FullPermissionTerm, Term}
import silAST.source.SourceLocation

/**
  * @author Christian Klauser
  */
trait ExpressionTranslator extends MemberEnvironment {
  protected def translateTerm(exprNode :  chalice.Expression ) : Term
  protected def translateClassRef(classRef : chalice.Class) : DataType
  protected def translatePermission(permission : chalice.Permission) : Term
  
  protected final def matchingExpression(partialFunction : PartialFunction[chalice.Expression, Expression]) : PartialFunction[chalice.Expression, Expression] = partialFunction

  def translateExpression(e : chalice.Expression) : Expression = expressionTranslation(e)

  protected def withWaitlevel(waitlevel : SourceLocation)(f : Function[Term,Expression]) = {
    // ∀ o:ref :: $CurrentThread.heldMap[o] ⇒ f($CurrentThread.muMap[o])
    val heldMap = currentExpressionFactory.makeFieldReadTerm(environmentCurrentThreadTerm(waitlevel),prelude.Thread.heldMap,waitlevel)
    val oVar = currentExpressionFactory.makeBoundVariable(getNextName("o"),referenceType,waitlevel)
    val oVarTerm = currentExpressionFactory.makeBoundVariableTerm(oVar,waitlevel)
    val isHeld = currentExpressionFactory.makeDomainPredicateExpression(prelude.Boolean.eval,TermSequence(
      currentExpressionFactory.makeDomainFunctionApplicationTerm(
        prelude.Map.HeldMap.get,TermSequence(heldMap,oVarTerm),waitlevel)
    ),waitlevel)
    val muMap = currentExpressionFactory.makeFieldReadTerm(environmentCurrentThreadTerm(waitlevel),prelude.Thread.muMap,waitlevel)
    val oMu = currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Map.MuMap.get,TermSequence(muMap,oVarTerm),waitlevel)
    val impl = currentExpressionFactory.makeBinaryExpression(Implication()(waitlevel),isHeld, f(oMu),waitlevel)
    currentExpressionFactory.makeQuantifierExpression(Forall()(waitlevel),oVar,impl)(waitlevel)
  }


  protected def eqWaitlevel(sameAsWaitlevel : SourceLocation,waitlevel : SourceLocation, other : chalice.Expression) : Expression = {
    // ∀ o:ref :: $CurrentThread.heldMap[o] ⇒ other == $CurrentThread.muMap[o]
    withWaitlevel(waitlevel){ oMu =>
      currentExpressionFactory.makeEqualityExpression(oMu,translateTerm(other),sameAsWaitlevel)
    }
  }

  protected def expressionTranslation : PartialFunction[chalice.Expression, Expression] = matchingExpression {
      case expression@chalice.Old(inner) =>
        currentExpressionFactory.makeOldExpression(translateExpression(inner))(expression)
      case sameAsWaitlevel@chalice.Eq(chalice.MaxLockLiteral(),chalice.MaxLockLiteral()) =>
        TrueExpression()(sameAsWaitlevel,List("waitlevel == waitlevel"))
      case sameAsWaitlevel@chalice.Eq(waitlevel@chalice.MaxLockLiteral(),other) =>
        eqWaitlevel(sameAsWaitlevel,waitlevel,other)
      case sameAsWaitlevel@chalice.Eq(other,waitlevel@chalice.MaxLockLiteral()) =>
        eqWaitlevel(sameAsWaitlevel,waitlevel,other)
      case expression@chalice.And(lhs,rhs) =>
        val lhsT = translateExpression(lhs)
        val rhsT = translateExpression(rhs)
        currentExpressionFactory.makeBinaryExpression(And()(expression),lhsT,rhsT,expression)
      case expression@chalice.Or(lhs,rhs) =>
        val lhsT = translateExpression(lhs)
        val rhsT = translateExpression(rhs)
        currentExpressionFactory.makeBinaryExpression(Or()(expression),lhsT,rhsT,expression)
      case expression@chalice.Implies(lhs,rhs) =>
        val lhsT = translateExpression(lhs)
        val rhsT = translateExpression(rhs)
        currentExpressionFactory.makeBinaryExpression(Implication()(expression),lhsT,rhsT,expression)
      case equality@chalice.Eq(lhs,rhs) => 
        val lhsTerm = translateTerm(lhs)
        val rhsTerm = translateTerm(rhs)
        currentExpressionFactory.makeEqualityExpression(lhsTerm,rhsTerm,equality)
      case inequality@chalice.Neq(lhs,rhs) =>
        val lhsTerm = translateTerm(lhs)
        val rhsTerm = translateTerm(rhs)
        val eq = currentExpressionFactory.makeEqualityExpression(lhsTerm,rhsTerm,inequality)
        currentExpressionFactory.makeUnaryExpression(Not()(inequality),eq,inequality)
      case ifThenElse@chalice.IfThenElse(cond,thn,els) =>
        val condExpr = translateExpression(cond)
        val thnExpr = translateExpression(thn)
        val elsExpr = translateExpression(els)
        val pos = currentExpressionFactory.makeBinaryExpression(Implication()(ifThenElse),condExpr,thnExpr,ifThenElse)
        val negCondExpr = currentExpressionFactory.makeUnaryExpression(Not()(ifThenElse),condExpr,ifThenElse)
        val neg = currentExpressionFactory.makeBinaryExpression(Implication()(ifThenElse),negCondExpr,elsExpr,ifThenElse)
        currentExpressionFactory.makeBinaryExpression(And()(ifThenElse),pos,neg,ifThenElse)
      case f@chalice.LockBelow(chalice.MaxLockLiteral(),chalice.MaxLockLiteral()) =>
        FalseExpression()(f,List("waitlevel << waitlevel")):Expression
      case aboveWaitlevel@chalice.LockBelow(waitlevel@chalice.MaxLockLiteral(),other) =>
        // ∀ o:ref :: $CurrentThread.heldMap[o] ⇒ $CurrentThread.muMap[o] << other
        withWaitlevel(waitlevel){ oMu =>
          currentExpressionFactory.makeDomainPredicateExpression(prelude.Mu().below,TermSequence(oMu,translateTerm(other)),aboveWaitlevel)
        }
      case belowWaitlevel@chalice.LockBelow(other,waitlevel@chalice.MaxLockLiteral()) =>
        // ∀ o:ref :: $CurrentThread.heldMap[o] ⇒ other << $CurrentThread.muMap[o]
        withWaitlevel(waitlevel){ oMu =>
          currentExpressionFactory.makeDomainPredicateExpression(prelude.Mu().below,TermSequence(translateTerm(other),oMu),belowWaitlevel)
        }
      case binary:chalice.BinaryExpr =>
        val (lhs,rhs) = (binary.E0,binary.E1)
        // binary.ExpectedXhsType is often null, use the "inferred" types for the operands instead
        val (lhsType,rhsType,resultType) = (translateClassRef(lhs.typ),translateClassRef(rhs.typ),translateClassRef(binary.ResultType))

        domainPredicateLookup.lookup(List(lhsType,rhsType,resultType).map(_.domain))(binary.OpName,List(Some(lhsType),Some(rhsType))) match {
          case Success(e) =>
            currentExpressionFactory.makeDomainPredicateExpression(e,TermSequence(translateTerm(lhs),translateTerm(rhs)),binary)
          case Ambiguous(ops) =>
            report(messages.OperatorNotFound(binary,lhsType,rhsType,resultType))
            dummyExpr(currentExpressionFactory,binary)
          case Failure() =>
            //Fall back to translating a boolean term and use Boolean.Eval
            val term = translateTerm(binary)
            if(term.dataType.isCompatible(prelude.Boolean.dataType)) {
              currentExpressionFactory.makeDomainPredicateExpression(prelude.Boolean.eval,TermSequence(term),binary)
            } else {
              report(messages.TermInExpressionPosition(binary,term.dataType))
              dummyExpr(currentExpressionFactory,binary)
            }
        }
      case expression@chalice.Access(chalice.MemberAccess(tokenExpr,joinableName), permission)
        if tokenExpr.typ.IsToken && joinableName == prelude.Token.joinable.name => {
          // translate `acc(token.joinable,X)` to `(acc(token.joinable,X) && acc(token.args/olds,X)...)`
          // i.e., make sure that arg/old fields always have the same permissions as token.joinable.
          val m = methods(tokenExpr.typ.asInstanceOf[chalice.TokenClass].method)
          val tokenTerm = translateTerm(tokenExpr)
          translateAccessExpression(permission){ permAmount =>
            m.callToken.allFields
              .map(currentExpressionFactory.makeFieldPermissionExpression(tokenTerm,_,permAmount,expression))
              .reduce[Expression](currentExpressionFactory.makeBinaryExpression(And()(expression),_,_,expression))
          }
        }
      case expression@chalice.Access(fieldAccess@chalice.MemberAccess(objectReference,muName),permission)
        if fieldAccess.typ.IsMu && muName == prelude.Object.mu.name => {
        // translate `acc(x.mu,p) to `acc(x.mu,p) && $CurrentThread.muMap[x] == x.mu`
        //  linking the muMap to the actual value of mu
        val muMap = currentExpressionFactory.makeFieldReadTerm(environmentCurrentThreadTerm(fieldAccess),prelude.Thread.muMap,fieldAccess)
        val objRefTerm = translateTerm(objectReference)
        val link = currentExpressionFactory.makeEqualityExpression(
          currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Map.MuMap.get,TermSequence(muMap,objRefTerm),fieldAccess),
          currentExpressionFactory.makeFieldReadTerm(objRefTerm,prelude.Object.mu,fieldAccess),expression
        )
        currentExpressionFactory.makeBinaryExpression(And()(expression),
          translateAccessExpression(permission)(currentExpressionFactory.makeFieldPermissionExpression(objRefTerm,prelude.Object.mu,_,expression)),
          link,expression
        )
      } : Expression
      case ma@chalice.MemberAccess(target,_) if ma.isPredicate =>
        currentExpressionFactory.makePredicatePermissionExpression(translateTerm(target),predicates(ma.predicate),FullPermissionTerm()(ma,Nil),ma)
      case expression@chalice.Access(memberAccess, permission) if !memberAccess.isPredicate =>
        translateAccessExpression(permission)(
          currentExpressionFactory.makeFieldPermissionExpression(translateTerm(memberAccess.e),fields(memberAccess.f),_,expression))
      case expression@chalice.Access(predicateAccess, permission) if predicateAccess.isPredicate =>
        translateAccessExpression(permission)(currentExpressionFactory.makePredicatePermissionExpression(
          translateTerm(predicateAccess.e),
          predicates(predicateAccess.predicate),
          _,
          expression))
      case unfolding@chalice.Unfolding(predicateAccess, body) =>
        val location = translateTerm(predicateAccess.ma.e)
        val permissionExpr = currentExpressionFactory.makePredicatePermissionExpression(location,
          predicates(predicateAccess.ma.predicate),
          translatePermission(predicateAccess.perm), unfolding)
        currentExpressionFactory.makeUnfoldingExpression(
          permissionExpr,translateExpression(body),unfolding)
      case boolExpr if boolExpr.typ == chalice.BoolClass =>
        val boolTerm = translateTerm(boolExpr)
        currentExpressionFactory.makeDomainPredicateExpression(prelude.Boolean.eval,TermSequence(boolTerm),boolExpr)

  } orElse missingTranslation
  
  protected def translateAccessExpression(permission : chalice.Permission)(body : Term => Expression) : Expression = permission match {
    case s@chalice.Star =>
      val ctor = new PureLanguageConstruct(this,s)
      import ctor._
      val readFractionVar = currentExpressionFactory.makeBoundVariable(getNextName("kstar"),permissionType,s)
      val readFractionTerm = currentExpressionFactory.makeBoundVariableTerm(readFractionVar,s)
      currentExpressionFactory.makeQuantifierExpression(Exists()(s),readFractionVar, conjunction(
        permissionLT.apply(noPermission, readFractionTerm),   // 0 < k 
        permissionLT.apply(readFractionTerm, fullPermission), // k < write
        permissionLT.apply(readFractionTerm, environmentReadFractionTerm(permission)), // k < k_method
        body(readFractionTerm)))(s)// body(k)
    case amount => body(translatePermission(amount))
  }

  protected def missingTranslation[E] = new PartialFunction[chalice.Expression,E] {
    def isDefinedAt(e : chalice.Expression) = false
    def apply(expression : chalice.Expression) = {
      report(messages.UnknownAstNode(expression))
      dummyExpr(currentExpressionFactory,expression).asInstanceOf[E]
    }
  }
}

