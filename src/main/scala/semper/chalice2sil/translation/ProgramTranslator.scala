package semper.chalice2sil.translation

import semper.sil.ast._
import semper.chalice2sil.util._
import scala.collection.mutable._
import java.lang.String
import semper.chalice2sil.messages._
import chalice.Frac

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
  val messages = new LinkedList[MessageId]  // messages generated in the translation
  val silEnvironment = new SILProgramEnvironment
    // contains all SIL members and local variables generated in the translation

  // translated invariants
  val silTranslatedInvariants = new HashMap[chalice.Class, Predicate]

  // translated symbols
  val symbolMap = new HashMap[chalice.ASTNode, Nodes]
    // maps Chalice class members and local variables to SIL members and local variables

  // global K permission for predicates and invariants
  val globalK = new LocalVar("globalK")  // YANNIS: todo: may/must I put this in the SIL program?

  // the set domain
  val typeVar = new TypeVar("X")
  val setDomain =
    new Domain("set", Nil, Nil, List(typeVar))
    // YANNIS todo: add set functions and axioms

  //val prelude = new ChalicePrelude(this)
  // YANNIS todo: fix ChalicePrelude

  def translate(decls : Seq[chalice.TopLevelDecl]) : (Program, LinkedList[MessageId]) = {
    decls.foreach(collectSymbols)
    decls.foreach(translate)
    (new Program(programName, List(setDomain), silEnvironment.silFields, silEnvironment.silFunctions,
      silEnvironment.silPredicates, silEnvironment.silMethods), messages)
  }
  
  protected def collectSymbols(decl : chalice.TopLevelDecl) { decl match {
    case c:chalice.Class if c.IsNormalClass => collectSymbols(c)
    case node => messages += UnknownAstNode(node)
  }}

  protected def collectSymbols(classNode : chalice.Class){
    classNode.members.view foreach  {
      case f:chalice.Field =>
        val newField = new Field(f.FullName, translateType(f.typ))(new SourcePosition(f.pos.line, f.pos.column))
        symbolMap(f) = newField
        silEnvironment.silFields += (f.FullName -> newField)
      case p:chalice.Predicate =>
        val newPredicate =
          new Predicate(p.FullName, List(new LocalVarDecl("this",Ref)))(new SourcePosition(p.pos.line, p.pos.column))
        symbolMap(p) = newPredicate
        silEnvironment.silPredicates += (p.FullName, newPredicate)
      case m:chalice.Method =>
        val myThis = new LocalVarDecl("this", Ref)
        val myK = new LocalVarDecl("PermK", Perm)
        val ins = myThis :: myK :: translateVars(m.ins)
        val newMethod = new Method(m.FullName, ins, translateVars(m.outs))(new SourcePosition(m.pos.line, m.pos.column))
        symbolMap(m) = newMethod
        silEnvironment.silMethods += (m.FullName, newMethod)
      case f:chalice.Function =>
        val myThis = new LocalVarDecl("this", Ref)
        val ins = myThis :: translateVars(f.ins)
        val newFunction = new Function(f.FullName, ins, null, null)(
          translateType(f.out), new SourcePosition(f.pos.line, f.pos.column)
        )
        symbolMap(f) = newFunction
        silEnvironment.silMethods += (f.FullName, newFunction)
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
    val ths = new LocalVarDecl("this")

    // Translate one member at a time
    classNode.members.foreach({
      case m: chalice.Method  => translateMethod(m)
      case p: chalice.Predicate => translatePredicate(p)
      case f: chalice.Function =>
        new FunctionTranslator(this, f).translate() // YANNIS todo: fix function translators
      case i: chalice.MonitorInvariant =>
        val silCurrent = translateExp(i, ths, globalK)
        val silPrevious = silTranslatedInvariants.get(classNode.FullName)
        val silNew = silPrevious match {
          case None => silCurrent
          case Some(p) => And(p, silCurrent)
        }
        silTranslatedInvariants(classNode.FullName) = silNew
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
        new DomainType(seqDomain, tvm)   // YANNIS: todo: fix
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
    cVars.foreach(x => result += new LocalVarDecl(x.UniqueName, translateType(x.t)))
    result
  }

  protected def translatePredicate(cPredicate: chalice.Predicate) = {
    val sPredicate = silEnvironment.silPredicates(cPredicate.FullName)
    val sThis = sPredicate.formalArgs(0)
    sPredicate.body = translateExp(cPredicate.definition, sThis, globalK)
  }

  protected def translateMethod(cMethod: chalice.Method) = {
    val sMethod = silEnvironment.silMethods(cMethod.FullName)
    val sThis = sMethod.ins(0)
    val sK = sMethod.ins(1)

    // translate specifications
    val silPreconditions = new LinkedList[Exp]
    val silPostConditions = new LinkedList[Exp]
    val locals = List(sMethod.formalArgs, sMethod.formalReturns)
    cMethod.spec.foreach {
      _ match {
        case chalice.Precondition(e) => silPreconditions +=
          translateExp(e, sThis)
        case chalice.PostCondition(e) => silPostConditions +=
          translateExp(e, sThis)
      }
    }
    sMethod.pres = silPreconditions
    sMethod.posts = silPostConditions

    // translate body
    translateBody(cMethod, sThis, sK)
  }

  protected def translateExp(cExp: chalice.Expression, myThis: LocalVarDecl, myK: LocalVarDecl) : Exp = {
    val position = new SourcePosition(cExp.pos.line, cExp.pos.column)
    cExp match {
      // old expression
      case chalice.Old(inner) => new Old(translateExp(inner, myThis))(position)

      // chalice2sil ignores all deadlock prevention specs
      case chalice.LockBelow(_,_) =>  new TrueLit()
      case chalice.Eq(chalice.MaxLockLiteral(),_) => new TrueLit()
      case chalice.Eq(_,chalice.MaxLockLiteral()) => new TrueLit()

      // logical operators
      case chalice.And(lhs, rhs) =>
        new And(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Or(lhs, rhs) =>
        new Or(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Implies(lhs, rhs) =>
        new Implies(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Eq(lhs, rhs) =>
        new EqCmp(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Neq(lhs, rhs) =>
        new NeCmp(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Not(op) => new Not(translateExp(op, myThis, myK))(position)
      case chalice.IfThenElse(cond, thn, els) =>
        new CondExp(
          translateExp(cond, myThis, myK), translateExp(thn, myThis, myK), translateExp(els, myThis, myK)
        )(position)

       // arithmetic operators
      case chalice.Plus(lhs, rhs) =>
        new Add(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Minus(lhs, rhs) =>
        new Sub(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Times(lhs, rhs) =>
        new Mul(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Div(lhs, rhs) =>
        new Div(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Mod(lhs, rhs) =>
        new Mod(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)

        // YANNIS: todo: arithmetic comparison operators

        // sequence operators
      case chalice.EmptySeq(t) => new EmptySeq(translateType(t))(position)
      case chalice.ExplicitSeq(elems) => new ExplicitSeq(elems map translateExp(_,myThis))(position)
      case chalice.SeqAccess(lhs, rhs) =>
        new RangeSeq(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Length(e) => new SeqLength(translateExp(e, myThis, myK))(position)
      case chalice.At(lhs, rhs) =>
        new SeqIndex(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Drop(lhs, rhs) =>
        new SeqDrop(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Take(lhs, rhs) =>
        new SeqTake(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)
      case chalice.Contains(lhs, rhs) =>
        new SeqContains(translateExp(lhs, myThis, myK), translateExp(rhs, myThis, myK))(position)

        // set operators: YANNIS: todo

        // member access
      case chalice.ThisExpr => myThis
      case ma@chalice.MemberAccess(e, id) =>
        val cls = e.typ.id
        /*val memb = silEnvironment.silFields(chaliceEnvironment.chaliceFields(cls+"::"+id).FullName)
        if (ma.isPredicate) new PredicateAccess(translateExp(e, locals), memb)(position)
        else new FieldAccess(translateExp(e, locals), memb)(position)*/
        // YANNIS: write this again, in the new refactoring
        // YANNIS: todo: case class BackPointerMemberAccess(ex: Expression, typeId: String, fieldId: String) extends Expression {}

        // access permissions
      case chalice.Access(ma, perm) =>
        val silma = translateExp(ma, myThis, myK)
        val silpe = translatePerm(perm, myThis, myK)
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

  protected def translateBody(cMethod: chalice.Method, myThis: LocalVarDecl, myK: LocalVarDecl) = {
    val sMethod = symbolMap(cMethod).asInstanceOf[Method]
    sMethod.body = new Seqn(cMethod.body.foreach(translateStm(_,myThis,myK)))
  }

  protected def translateStm(cStm: chalice.Statement, myThis: LocalVarDecl, myK: LocalVarDecl) : Stmt = {
    val position = new SourcePosition(cStm.pos.line, cExp.pos.column)
    st match {
      case chalice.Assert(e) => Assert(translateExp(e, myThis, myK))(position)
      case chalice.Assume(e) => Inhale(translateExp(e, myThis, myK))(position)
    }
    // YANNIS: todo
    /*

      case class BlockStmt(ss: List[Statement]) extends Statement {
    override def Targets = (ss :\ Set[Variable]()) { (s, vars) => vars ++ s.Targets}
    }
      case class IfStmt(guard: Expression, thn: BlockStmt, els: Option[Statement]) extends Statement {
    override def Targets = thn.Targets ++ (els match {case None => Set(); case Some(els) => els.Targets})
    }
      case class WhileStmt(guard: Expression,
    oldInvs: List[Expression], newInvs: List[Expression], lkch: List[Expression],
    body: BlockStmt) extends Statement {
    val Invs = oldInvs ++ newInvs
    var LoopTargets: List[Variable] = Nil
    override def Targets = body.Targets
    }
      case class Assign(lhs: VariableExpr, rhs: RValue) extends Statement {
    override def Targets = if (lhs.v != null) Set(lhs.v) else Set()
    }
      case class FieldUpdate(lhs: MemberAccess, rhs: RValue) extends Statement
      case class LocalVar(v: Variable, rhs: Option[RValue]) extends Statement {
    override def Declares = List(v)
    override def Targets = rhs match {case None => Set(); case Some(_) => Set(v)}
    }
      case class Call(declaresLocal: List[Boolean], lhs: List[VariableExpr], obj: Expression, id: String, args: List[Expression]) extends Statement {
    var locals = List[Variable]()
    var m: Callable = null
    override def Declares = locals
    override def Targets = (lhs :\ Set[Variable]()) { (ve, vars) => if (ve.v != null) vars + ve.v else vars }
    }
      case class SpecStmt(lhs: List[VariableExpr], locals:List[Variable], pre: Expression, post: Expression) extends Statement {
    override def Declares = locals
    override def Targets = (lhs :\ Set[Variable]()) { (ve, vars) => if (ve.v != null) vars + ve.v else vars }
    }
      case class Install(obj: Expression, lowerBounds: List[Expression], upperBounds: List[Expression]) extends Statement
      case class Share(obj: Expression, lowerBounds: List[Expression], upperBounds: List[Expression]) extends Statement
      case class Unshare(obj: Expression) extends Statement
      case class Acquire(obj: Expression) extends Statement
      case class Release(obj: Expression) extends Statement
      case class RdAcquire(obj: Expression) extends Statement
      case class RdRelease(obj: Expression) extends Statement
      case class Downgrade(obj: Expression) extends Statement
      case class Lock(obj: Expression, b: BlockStmt, rdLock: Boolean) extends Statement {
    override def Targets = b.Targets
    }
      case class Free(obj: Expression) extends Statement
      case class CallAsync(declaresLocal: Boolean, lhs: VariableExpr, obj: Expression, id: String, args: List[Expression]) extends Statement {
    var local: Variable = null
    var m: Method = null
    override def Declares = if (local != null) List(local) else Nil
    override def Targets = if (lhs != null && lhs.v != null) Set(lhs.v) else Set()
    }
      case class JoinAsync(lhs: List[VariableExpr], token: Expression) extends Statement {
    var m: Method = null
    }
      case class Wait(obj: Expression, id: String) extends Statement {
    var c: Condition = null
    }
      case class Signal(obj: Expression, id: String, all: Boolean) extends Statement {
    var c: Condition = null
    }
      case class Send(ch: Expression, args: List[Expression]) extends Statement {
    }
      case class Receive(declaresLocal: List[Boolean], ch: Expression, outs: List[VariableExpr]) extends Statement {
    var locals = List[Variable]()
    override def Declares = locals
    override def Targets = (outs :\ Set[Variable]()) { (ve, vars) => if (ve.v != null) vars + ve.v else vars }
    }
      case class Fold(pred: Access) extends Statement
      case class Unfold(pred: Access) extends Statement

*/
  }


  protected def translatePerm(perm: chalice.Permission, myThis: LocalVarDecl, myK: LocalVarDecl) = {
    perm match {
      case chalice.Full => new FullPerm() // 100% permission
      case chalice.Frac(n) => new FractionalPerm(translateExp(n, myThis, myK), new IntLit(100)) // n% permission
      case chalice.Star => new WildcardPerm() // rd* permission
      case chalice.Epsilon => myK // the k permission parameter given to the method
        // attention: chalice.Epsilon is a misnomer!!
        // YANNIS: todo: separate LocalVarDecl from LocalVar everywhere
      case chalice.MethodEpsilon => myK // YANNIS: todo: what is the difference between chalice.Epsilon and this?

      // for predicate and monitor k permissions, we use one global k value
      case chalice.PredicateEpsilon => globalK
      case chalice.MonitorEpsilon => globalK
        // YANNIS: todo: deal with: case class ForkEpsilon(token: Expression) extends Write

      // counting permissions
      case chalice.Epsilons(n) => IntPermMul(translateExp(n, myThis, myK), new EpsilonPerm()) // n*ε permissions
    }
/*
    case class PermTimes(val lhs: Permission, val rhs: Permission) extends ArithmeticPermission {
      override def permissionType = {
        if (lhs.permissionType == rhs.permissionType) lhs.permissionType
        else Mixed
      }
    }
    case class IntPermTimes(val lhs: Expression, val rhs: Permission) extends ArithmeticPermission {
      override def permissionType = rhs.permissionType
    }
    case class PermPlus(val lhs: Permission, val rhs: Permission) extends ArithmeticPermission {
      override def permissionType = {
        if (lhs.permissionType == rhs.permissionType) lhs.permissionType
        else Mixed
      }
    }
    case class PermMinus(val lhs: Permission, val rhs: Permission) extends ArithmeticPermission {
      override def permissionType = {
        if (lhs.permissionType == rhs.permissionType) lhs.permissionType
        else Mixed
      }
    }
*/
  }
  // YANNIS: todo
}