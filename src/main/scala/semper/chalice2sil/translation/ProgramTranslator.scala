package semper.chalice2sil.translation

import semper.sil.ast._
import semper.chalice2sil.util._
import scala.collection.mutable._
import java.lang.String
import semper.chalice2sil.messages._

/**
 * Author: Christian Klauser
 * Code modified by Yannis Kassios
 */

// YANNIS: todo: fix Chalice resolution phase for general universal quantification and backpointers

class ProgramTranslator(val programOptions: semper.chalice2sil.ProgramOptions, val programName: String)
{
   // output of the translator
  val messages = new LinkedList[MessageId]
  val silEnvironment = new SILProgramEnvironment

  // sequence and set domains
  val typeVar = new TypeVar("X")
  val seqDomain =
    new Domain("seq", Nil, Nil, List(typeVar))
    // YANNIS todo: add sequence functions and axioms
  val setDomain =
    new Domain("set", Nil, Nil, List(typeVar))
    // YANNIS todo: add set functions and axioms


  // symbols of the Chalice program
  val chaliceEnvironment = new ChaliceProgramEnvironment

  /*val monitorInvariants = new DerivedFactoryCache[chalice.Class, String, PredicateTranslator] {
    protected def deriveKey(c : chalice.Class) = fullMonitorInvariantName(c)
    protected def construct(c : chalice.Class) = new MonitorInvariantPredicateTranslator(programTranslator,c,getNextId)
  }*/
  // YANNIS: todo: how are monitor invariants translated?

  val prelude = new ChalicePrelude(this)
  // YANNIS todo: fix ChalicePrelude

  def translate(decls : Seq[chalice.TopLevelDecl]) : (Program, LinkedList[MessageId]) = {
    decls.foreach(collectSymbols)
    decls.foreach(translate)
    (new Program(programName, List(seqDomain, setDomain), silEnvironment.silFields, silEnvironment.silFunctions,
      silEnvironment.silPredicates, silEnvironment.silMethods), messages)
  }
  
  protected def collectSymbols(decl : chalice.TopLevelDecl) { decl match {
    case c:chalice.Class if c.IsNormalClass => collectSymbols(c)
    case node => messages += UnknownAstNode(node)
  }}

  protected def collectSymbols(classNode : chalice.Class){
    classNode.members.view foreach  {
      case f:chalice.Field =>
        chaliceEnvironment.chaliceFields += (f.id, f)
        silEnvironment.silFields += (f.FullName, translateType(f.typ))
      case p:chalice.Predicate =>
        chaliceEnvironment.chalicePredicates += (p.id, p)
        silEnvironment.silPredicates += (p.FullName, new Predicate(p.FullName, new LocalVarDecl("this", Ref))(
          new SourcePosition(p.pos.line, p.pos.column)
        ))
      case m:chalice.Method =>
        val ins = translateVars(m.ins)
        ins += LocalVarDecl("this", Ref)
        chaliceEnvironment.chaliceMethods += (m.id, m)
        silEnvironment.silMethods +=
          (m.FullName, new Method(m.FullName, ins, translateVars(m.outs), null, null, null)(
              new SourcePosition(m.pos.line, m.pos.column)
          ))
      case f:chalice.Function =>
        val ins = translateVars(f.ins)
        ins += LocalVarDecl("this", Ref)
        chaliceEnvironment.chaliceFunctions += (f.id, f)
        silEnvironment.silMethods +=
          (f.FullName, new Function(f.FullName, ins, null, null)(
            translateType(f.out), new SourcePosition(f.pos.line, f.pos.column)
          ))
      case i:chalice.MonitorInvariant =>  // makes sure that invariant is created lazily
      case _ => // ignore other symbols
    }
  }

  protected def translate(decl : chalice.TopLevelDecl) {decl match {
    case c:chalice.Class if c.IsNormalClass => translate(c)
    case c:chalice.Channel => messages += ChannelsNotImplemented(c)
    case node => messages += UnknownAstNode(node)
  }}


  protected def translate(classNode: chalice.Class) = {
    // In Chalice2SIL, monitor invariants are not considered "members of a class"
    //  they are a "property of a class"

    // Translate one member at a time
    classNode.members.foreach({
      case m: chalice.Method  => translateMethod(m)
      case p: chalice.Predicate =>
        new PredicateTranslator(this, p).translate() // YANNIS todo: fix predicate translators
      case f: chalice.Function =>
        new FunctionTranslator(this, f).translate() // YANNIS todo: fix function translators
      case i: chalice.MonitorInvariant => () // YANNIS: todo: how are monitor invariants treated?
      case otherNode => messages += UnknownAstNode(otherNode)
    })
  }

  protected def translateType(cType: chalice.Type) : Type = {
    cType.id match {
      case "seq" =>
        if(typ.params.length != 1) { messages += WrongNumberOfTypeParameters; Int }
        val tvm = new HashMap[TypeVar, Type]
        val silT = translateType(cType.params.head)
        tvm += (typeVar, silT)
        new DomainType(seqDomain, tvm)
      case "set" =>
        if(typ.params.length != 1) { messages += WrongNumberOfTypeParameters; Int }
        val tvm = new HashMap[TypeVar, Type]
        val silT = translateType(cType.params.head)
        tvm += (typeVar, silT)
        new DomainType(setDomain, tvm)
      case "int" => Int
      case "bool" => Bool
      case _ => Ref
    }
  }

  protected def translateVars(cVars: List[chalice.Variable]) = {
    val result = new LinkedList[LocalVarDecl]
    cVars.foreach(x => result += new LocalVarDecl(x.UniqueName, translateType(x.t)))
    result
  }

  protected def translateMethod(cMethod: chalice.Method) = {
    val sMethod = silEnvironment.silMethods[cMethod.FullName]

    // translate specifications
    val silPreconditions = new LinkedList[Exp]
    val silPostConditions = new LinkedList[Exp]
    cMethod.spec.foreach {
      _ match {
        case chalice.Precondition(e) => silPreconditions +=
          translateExp(e, List(sMethod.formalArgs, sMethod.formalReturns))
        case chalice.PostCondition(e) => silPostConditions +=
          translateExp(e, List(sMethod.formalArgs, sMethod.formalReturns))
      }
    }
    sMethod.pres = silPreconditions
    sMethod.posts = silPostConditions

    // translate body
    translateBody(cMethod, sMethod)
  }

  protected def translateExp(cExp: chalice.Expression, locals: Seq[Map[String,LocalVarDecl]]) : Exp = {
    val position = new SourcePosition(cExp.pos.line, cExp.pos.column)
    cExp match {
      // old expression
      case chalice.Old(inner) => new Old(translateExp(inner, locals))(position)

      // chalice2sil ignores all deadlock prevention specs
      case chalice.LockBelow(_,_) =>  new TrueLit()
      case chalice.Eq(chalice.MaxLockLiteral(),_) => new TrueLit()
      case chalice.Eq(_,chalice.MaxLockLiteral()) => new TrueLit()

      // logical operators
      case chalice.And(lhs, rhs) =>
        new And(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Or(lhs, rhs) =>
        new Or(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Implies(lhs, rhs) =>
        new Implies(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Eq(lhs, rhs) =>
        new EqCmp(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Neq(lhs, rhs) =>
        new NeCmp(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Not(op) => new Not(translateExp(op, locals))(position)
      case chalice.IfThenElse(cond, thn, els) =>
        new CondExp(translateExp(cond, locals), translateExp(thn, locals), translateExp(els, locals))(position)

        // YANNIS: todo: finish this
      /*

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
*/

    }
  }

  protected def translateBody(cMethod: chalice.Method, sMethod: Method) = {}
  // YANNIS: todo
}