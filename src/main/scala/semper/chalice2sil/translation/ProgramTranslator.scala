package semper.chalice2sil.translation

import semper.sil.ast._
import semper.chalice2sil.util._
import scala.collection.mutable._
import java.lang.String
import semper.chalice2sil.messages._
import javax.swing.plaf.multi.MultiOptionPaneUI

/**
 * Author: Christian Klauser
 * Code modified by Yannis Kassios
 */

// YANNIS: todo: fix Chalice resolution phase for general universal quantification and backpointers

// YANNIS: todo: make the program running

// YANNIS: todo: run the correct pretty-printer and check all program functionality

class ProgramTranslator(val programOptions: semper.chalice2sil.ProgramOptions, val programName: String)
{
   // output of the translator
  val messages = new LinkedList[MessageId]
  val silEnvironment = new SILProgramEnvironment

  // sequence and set domains
  val typeVar = new TypeVar("X")
  val seqDomain =
    new Domain("seq", Nil, Nil, List(typeVar))
    // YANNIS todo: add sequence functions and axioms (or maybe remove this domain altogether?)
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
        chaliceEnvironment.chaliceFields += (classNode.id+"::"+f.id, f)
        silEnvironment.silFields += (f.FullName, translateType(f.typ))
      case p:chalice.Predicate =>
        chaliceEnvironment.chalicePredicates += (classNode.id+"::"+p.id, p)
        silEnvironment.silPredicates += (p.FullName, new Predicate(p.FullName, new LocalVar("this")(Ref))(
          new SourcePosition(p.pos.line, p.pos.column)
        ))
      case m:chalice.Method =>
        val ins = translateVars(m.ins)
        ins += LocalVar("this")(Ref)
        chaliceEnvironment.chaliceMethods += (classNode.id+"::"+m.id, m)
        silEnvironment.silMethods +=
          (m.FullName, new Method(m.FullName, ins, translateVars(m.outs), null, null, null)(
              new SourcePosition(m.pos.line, m.pos.column)
          ))
      case f:chalice.Function =>
        val ins = translateVars(f.ins)
        ins += LocalVar("this")(Ref)
        chaliceEnvironment.chaliceFunctions += (classNode.id+"::"+f.id, f)
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

    // YANNIS: todo: what about permission types?
  }

  protected def translateVars(cVars: List[chalice.Variable]) = {
    val result = new LinkedList[LocalVar]
    cVars.foreach(x => result += new LocalVar(x.UniqueName)(translateType(x.t)))
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

  protected def translateExp(cExp: chalice.Expression, locals: Seq[Map[String,LocalVar]]) : Exp = {
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

       // arithmetic operators
      case chalice.Plus(lhs, rhs) =>
        new Add(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Minus(lhs, rhs) =>
        new Sub(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Times(lhs, rhs) =>
        new Mul(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Div(lhs, rhs) =>
        new Div(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Mod(lhs, rhs) =>
        new Mod(translateExp(lhs, locals), translateExp(rhs, locals))(position)

        // YANNIS: todo: arithmetic comparison operators

        // sequence operators
      case chalice.EmptySeq(t) => new EmptySeq(translateType(t))(position)
      case chalice.ExplicitSeq(elems) => new ExplicitSeq(elems map translateExp(_,locals))(position)
      case chalice.SeqAccess(lhs, rhs) =>
        new RangeSeq(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Length(e) => new SeqLength(translateExp(e, locals))(position)
      case chalice.At(lhs, rhs) =>
        new SeqIndex(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Drop(lhs, rhs) =>
        new SeqDrop(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Take(lhs, rhs) =>
        new SeqTake(translateExp(lhs, locals), translateExp(rhs, locals))(position)
      case chalice.Contains(lhs, rhs) =>
        new SeqContains(translateExp(lhs, locals), translateExp(rhs, locals))(position)

        // set operators: YANNIS: todo

        // member access
      case chalice.ThisExpr => locals[0]["this"]
      case ma@chalice.MemberAccess(e, id) =>
        val cls = e.typ.id
        val memb = silEnvironment.silFields(chaliceEnvironment.chaliceFields(cls+"::"+id).FullName)
        if (ma.isPredicate) new PredicateAccess(translateExp(e, locals), memb)(position)
        else new FieldAccess(translateExp(e, locals), memb)(position)
        // YANNIS: todo: case class BackPointerMemberAccess(ex: Expression, typeId: String, fieldId: String) extends Expression {}

        // access permissions
      case chalice.Access(ma, perm) =>
        val silma = translateExp(ma, locals)
        val silpe = translatePerm(perm, locals)
        if (ma.isPredicate) new PredicateAccessPredicate(silma, silpe)(position)
        else new FieldAccessPredicate(silma, silpe)(position)

/*        YANNIS: todo the following cases
        case class BackPointerAccess(ma: BackPointerMemberAccess, var perm: Permission) extends PermissionExpr(perm)
        case class AccessAll(obj: Expression, var perm: Permission) extends WildCardPermission(perm)
        case class AccessSeq(s: Expression, f: Option[MemberAccess], var perm: Permission) extends WildCardPermission(perm)*/

        // YANNIS: todo: finish the method

     /*

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

  protected def translatePerm(perm: chalice.Permission, locals: Seq[Map[String,LocalVar]]) = { null }
  // YANNIS: todo
}