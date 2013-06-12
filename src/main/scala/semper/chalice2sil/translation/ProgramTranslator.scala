package semper.chalice2sil.translation

import semper.sil.ast._
import semper.chalice2sil.util._
import scala.collection._
import java.lang.String
import semper.chalice2sil.messages._
import semper.chalice2sil.util.SetDomain
import chalice.TypeQuantification

/**
 * Author: Yannis Kassios (based on an older version by Christian Klauser)
 */

// todo: run the correct pretty-printer and test all program functionality

// todo: use name generator for all local variable declarations

// todo: output messages to the console when appropriate

// todo: fix error messages

class ProgramTranslator(val programOptions: semper.chalice2sil.ProgramOptions, val programName: String)
{
   // output of the translator
  val messages = scala.collection.mutable.ListBuffer[semper.chalice2sil.Message]()  // messages generated in the translation
  val silEnvironment = new SILProgramEnvironment()
    // contains all SIL members and local variables generated in the translation
    // YANNIS: todo refactor to eliminate this class

  // translated invariants
  val silTranslatedInvariants = new scala.collection.mutable.HashMap[chalice.Class, Predicate]()

  // translated symbols
  val symbolMap = new scala.collection.mutable.HashMap[chalice.ASTNode, Node]()
    // maps Chalice class members and local variables to SIL members and local variables

  // name generator -- ensures uniqueness and validity of names of local variables
  val nameGenerator = new semper.sil.utility.SilNameGenerator

  // this domain introduces the constant K permission for use in monitor invariants
  // YANNIS todo: axiom: the K permission is read-only
  val globalK = DomainFunc(nameGenerator.createIdentifier("globalK"), Seq(), Perm, true)()
  val GlobalKPermissionDomain = Domain("GlobalKPermission", Seq(globalK), Seq(), Seq())()

  def translate(decls : Seq[chalice.TopLevelDecl]) : (Program, Seq[semper.chalice2sil.Message]) = {
    decls.foreach(collectSymbols)
    decls.foreach(translate)
    (Program(List(GlobalKPermissionDomain), silEnvironment.silFields.values.toSeq,
      silEnvironment.silFunctions.values.toSeq, silEnvironment.silPredicates.values.toSeq,
      silEnvironment.silMethods.values.toSeq)(), messages.toSeq)
  }
  
  protected def collectSymbols(decl : chalice.TopLevelDecl) { decl match {
    case c:chalice.Class if c.IsNormalClass => collectSymbols(c)
    case node => messages += UnknownAstNode(node)
  }}

  protected def collectSymbols(classNode : chalice.Class){
    classNode.members.view foreach  {
      case f:chalice.Field =>
        val newField = Field(f.FullName, translateType(f.typ))(SourcePosition(null, f.pos.line, f.pos.column))
        symbolMap(f) = newField
        silEnvironment.silFields += (f.FullName -> newField)
      case p:chalice.Predicate =>
        val ths = nameGenerator.createIdentifier("this")
        val newPredicate =
          Predicate(p.FullName, LocalVarDecl(ths, Ref)(), null)(SourcePosition(null, p.pos.line, p.pos.column))
        symbolMap(p) = newPredicate
        silEnvironment.silPredicates += (p.FullName -> newPredicate)
      case m:chalice.Method =>
        val ths = nameGenerator.createIdentifier("this")
        val k = nameGenerator.createIdentifier("k")
        val myThis = LocalVarDecl(ths, Ref)()
        val myK = LocalVarDecl(k, Perm)()
        val ins = myThis :: myK :: translateVars(m.ins)
        val newMethod = Method(m.FullName, ins, translateVars(m.outs), null, null, null, null)(
          SourcePosition(null, m.pos.line, m.pos.column))
        symbolMap(m) = newMethod
        silEnvironment.silMethods += (m.FullName -> newMethod)
      case f:chalice.Function =>
        val ths = nameGenerator.createIdentifier("this")
        val myThis = LocalVarDecl(ths, Ref)()
        val ins = myThis :: translateVars(f.ins)
        val newFunction = Function(f.FullName, ins, translateType(f.out), null, null, null)(
          SourcePosition(null, f.pos.line, f.pos.column))
        symbolMap(f) = newFunction
        silEnvironment.silFunctions += (f.FullName -> newFunction)
      case _ =>
    }
  }

  protected def translate(decl : chalice.TopLevelDecl) {decl match {
    case c:chalice.Class if c.IsNormalClass => translate(c)
    case c:chalice.Channel => messages += ChannelsNotImplemented(c)
    case node => messages += UnknownAstNode(node)
  }}

  protected def translate(classNode: chalice.Class) = {
    // A "this" declaration for the invariants
    val ths = LocalVarDecl(nameGenerator.createIdentifier("this"), Ref)()

    // An expression to store the monitor invariant
    var monitorInvariant : Exp = TrueLit()()

    // Translate one member at a time
    classNode.members.foreach({
      case m: chalice.Method  => translateMethod(m)
      case p: chalice.Predicate => translatePredicate(p)
      case f: chalice.Function => translateFunction(f)
      case i: chalice.MonitorInvariant =>
        val currentInv = translateExp(i.e, ths, PredicatePermissionTranslator(globalK))
        monitorInvariant = And(currentInv, monitorInvariant)()
      case otherNode => messages += UnknownAstNode(otherNode)
    })

    // YANNIS: todo: invariant is translated into a SIL expression monitorInvariant.  Turn it into a SIL predicate
      // and put it into silTranslatedInvariants(classNode.FullName)
  }

  protected def translateType(cType: chalice.Type) : Type = {
    cType.id match {
      case "seq" =>
        // YANNIS: todo: fix. seqs are now built in
 /*       if(typ.params.length != 1) { messages += WrongNumberOfTypeParameters; Int }
        val tvm = Map[TypeVar, Type]()
        val silT = translateType(cType.params.head)
        tvm += (typeVar, silT)
        new DomainType(seqDomain, tvm)*/
        null
      case "set" =>
        // YANNIS: todo: fix. sets are now built in
/*        if(typ.params.length != 1) { messages += WrongNumberOfTypeParameters; Int }
        else {
          val silT = translateType(cType.params.head)
          val tvm = Map(SetDomain.typeVar -> silT)
          tvm += (typeVar, silT)
          new DomainType(SetDomain, tvm)
        }*/
        null
      case "int" => Int
      case "bool" => Bool
      case "$Permission" => Perm
      case _ => Ref
    }
  }

  protected def translateVars(cVars: Seq[chalice.Variable]) = {
    val result = scala.collection.mutable.ListBuffer[LocalVarDecl]()
    cVars.foreach(x => result += LocalVarDecl(nameGenerator.createIdentifier(x.UniqueName), translateType(x.t))())
    result.toList
  }

  protected def translatePredicate(cPredicate: chalice.Predicate) = {
    val sPredicate = silEnvironment.silPredicates(cPredicate.FullName)
    val sThis = sPredicate.formalArg
    sPredicate.body = translateExp(cPredicate.definition, sThis, PredicatePermissionTranslator(globalK))
  }

  protected def translateFunction(cFunction: chalice.Function) = {
    val sFunction = silEnvironment.silFunctions(cFunction.FullName)
    val sThis = sFunction.formalArgs(0)

    // translate specifications
    val silPreconditions = scala.collection.mutable.ListBuffer[Exp]()
    val silPostConditions = scala.collection.mutable.ListBuffer[Exp]()
    cFunction.spec.foreach {
      _ match {
        case chalice.Precondition(e) => silPreconditions +=
          translateExp(e, sThis, FunctionPermissionTranslator())
        case chalice.Postcondition(e) => silPostConditions +=
          translateExp(e, sThis, FunctionPermissionTranslator())
      }
    }
    sFunction.pres = silPreconditions.toSeq
    sFunction.posts = silPostConditions.toSeq

    // translate body
    cFunction.definition match {
      case Some(body) => sFunction.exp = translateExp(body, sThis, FunctionPermissionTranslator())
    }
  }

  protected def translateMethod(cMethod: chalice.Method) = {
    val sMethod = silEnvironment.silMethods(cMethod.FullName)
    val sThis = sMethod.formalArgs(0)
    val sK = sMethod.formalArgs(1)
    val permTranslator = MethodPermissionTranslator(sK)

    // translate specifications
    val silPreconditions = scala.collection.mutable.LinkedList[Exp]()
    val silPostConditions = scala.collection.mutable.LinkedList[Exp]()
    cMethod.spec.foreach {
      _ match {
        case chalice.Precondition(e) =>
          silPreconditions.append(scala.collection.mutable.LinkedList(translateExp(e, sThis, permTranslator)))
        case chalice.Postcondition(e) =>
          silPostConditions.append(scala.collection.mutable.LinkedList(translateExp(e, sThis, permTranslator)))
      }
    }
    sMethod.pres = silPreconditions.toSeq
    sMethod.posts = silPostConditions.toSeq

    // translate body
    translateBody(cMethod, sThis, permTranslator)
  }

  protected def translateExp(cExp: chalice.Expression, myThis: LocalVarDecl, pTrans: PermissionTranslator) : Exp = {
    val position = SourcePosition(null, cExp.pos.line, cExp.pos.column)
    cExp match {
      // old expression
      case chalice.Old(inner) => Old(translateExp(inner, myThis, pTrans))(position)

      // chalice2sil ignores all deadlock prevention specs; YANNIS: todo: fix later
      case chalice.LockBelow(_,_) =>  TrueLit()(position)
      case chalice.Eq(chalice.MaxLockLiteral(),_) => TrueLit()(position)
      case chalice.Eq(_,chalice.MaxLockLiteral()) => TrueLit()(position)

      // logical operators
      case chalice.And(lhs, rhs) =>
        And(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)
      case chalice.Or(lhs, rhs) =>
        Or(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)
      case chalice.Implies(lhs, rhs) =>
        Implies(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)
      case chalice.Eq(lhs, rhs) =>
        EqCmp(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)
      case chalice.Neq(lhs, rhs) =>
        NeCmp(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)
      case chalice.Not(op) => Not(translateExp(op, myThis, pTrans))(position)
      case chalice.IfThenElse(cond, thn, els) =>
        CondExp(
          translateExp(cond, myThis, myK), translateExp(thn, myThis, pTrans), translateExp(els, myThis, pTrans)
        )(position)

       // arithmetic operators and set union, subtraction, intersection
      case chalice.Plus(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) Add(l, r)(position)
        else { // if lhs is not int, this translates to set union
          var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.union, List(l, r), t)
        }
      case chalice.Minus(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) Sub(l, r)(position)
        else { // if lhs is not int, this translates to set subtraction
          var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.subtraction, List(l, r), t)
        }
      case chalice.Times(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) Mul(l, r)(position)
        else { // if lhs is not int, this translates to set intersection
          var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.intersection, List(l, r), t)
        }
      case chalice.Div(lhs, rhs) =>
        Div(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)
      case chalice.Mod(lhs, rhs) =>
        Mod(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)

        // equality and inequality
      case chalice.Eq(lhs, rhs) =>
        EqCmp(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)
      case chalice.Neq(lhs, rhs) =>
        NeCmp(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)

        // arithmetic and set comparison operators
        // YANNIS: todo: sets are now built in
      case chalice.Less(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) LtCmp(l, r)(position)
        else { // if lhs is not int, this translates to set proper inclusion
          var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.subset, List(l, r), t)
        }
      case chalice.AtMost(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) LeCmp(l, r)(position)
        else { // if lhs is not int, this translates to set proper inclusion
          var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.subsetEq, List(l, r), t)
        }
      case chalice.AtLeast(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) GeCmp(l, r)(position)
        else { // if lhs is not int, this translates to set proper inclusion
          var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.supsetEq, List(l, r), t)
        }
      case chalice.Greater(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) GtCmp(l, r)(position)
        else { // if lhs is not int, this translates to set proper inclusion
          var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.supset, List(l, r), t)
        }

        // sequence operators
      case chalice.EmptySeq(t) => EmptySeq(translateType(t))(position)
      case chalice.ExplicitSeq(elems) => ExplicitSeq(elems map translateExp(_,myThis, pTrans))(position)
      case chalice.SeqAccess(lhs, rhs) =>
        new RangeSeq(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)
      case chalice.Length(e) => SeqLength(translateExp(e, myThis, pTrans))(position)
      case chalice.At(lhs, rhs) =>
        SeqIndex(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)
      case chalice.Drop(lhs, rhs) =>
        SeqDrop(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)
      case chalice.Take(lhs, rhs) =>
        SeqTake(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)
      case chalice.Contains(lhs, rhs) =>
        SeqContains(translateExp(lhs, myThis, pTrans), translateExp(rhs, myThis, pTrans))(position)

        // set operators: YANNIS: todo

        // member access
      case chalice.ThisExpr => myThis
      case ma@chalice.MemberAccess(e, id) =>
        val cls = e.typ.id
        if(cls!=null)
        {
          val f = cls.LookupMember(id)
          val isField = f.isInstanceOf[chalice.Field]
          if(isField || f.isInstanceOf[chalice.Predicate]) {
              val sf = symbolMap.getOrElse(f, null)
              if (sf == null) { messages += TypeError(e) ; null }
              else {
                if(isField) FieldAccess(translateExp(e, myThis, pTrans), sf)(position)
                else PredicateAccess(translateExp(e, myThis, pTrans), sf)(position)
              }
          }
          else { messages += TypeError(e) ; null }
        }
        else { messages += TypeError(e) ; null }
        // YANNIS: todo: case class BackPointerMemberAccess(ex: Expression, typeId: String, fieldId: String) extends Expression {}

        // access permissions
      case chalice.Access(ma, perm) =>
        val silma = translateExp(ma, myThis, pTrans)
        val silpe = pTrans(perm)
        if (ma.isPredicate) PredicateAccessPredicate(silma, silpe)(position)
        else FieldAccessPredicate(silma, silpe)(position)
      case chalice.BackPointerAccess(ma, perm) =>
        val silma = translateExp(ma, myThis, pTrans)
        val silpe = pTrans(perm)
        FieldAccessPredicate(silma, silpe)(position)

      case chalice.AccessAll(obj, perm) =>
        val silo = translateExp(o, myThis, pTrans)
        val silpe = pTrans(perm)
        var silexp : Exp = TrueLit

        if(obj==null || obj.typ == null) messages += UnknownAstNode(obj)
        else {
            // add access to all declared fields and predicates
          obj.typ.DeclaredFields.foreach{ f =>
            val sf = symbolMap.getOrElse(f, null)
            if (sf == null) messages += TypeError(obj)
            else silexp = And(silexp, FieldAccessPredicate(FieldAccess(silo, sf), silpe))
          }

           // YANNIS: todo: does this cover predicates or only fields?

           // YANNIS: todo: add access to backpointer fields

         }

           // return the complete access permission expression
         silexp

        case chalice.AccessSeq(seq, member, perm) =>
          val silseq = translateExp(seq, myThis, pTrans)
          val silpe = pTrans(perm)
          var permissionPerObject : Exp => Exp
            // this closure will return the permission expression for each element of the sequence

            // define permissionPerObject
          member match {
            case None =>
              if(obj==null || obj.typ==null) messages += UnknownAstNode(obj)
              else {
                  permissionPerObject = (exp =>
                    seq.typ(0).DeclaredFields.foreach{
                      case f: chalice.Field =>
                        val sf = symbolMap.getOrElse(f, null)
                        if (sf == null) messages += TypeError(obj)
                        else silexp = And(silexp, FieldAccessPredicate(FieldAccess(exp, sf), silpe))
                    case p: chalice.Predicate =>
                        val sp = symbolMap.getOrElse(p, null)
                        if (sp == null) messages += TypeError(obj)
                        else silexp = And(silexp, PredicateAccessPredicate(PredicateAccess(exp, sp), silpe))
                    case _ => UnknownAstNode(exp)
                  }
                    // YANNIS: todo: add access to backpointer fields
                )
              }
            case Some(f) =>
              if(f.isPredicate) permissionPerObject = (exp => PredicateAccessPredicate(exp, silpe)(position))
              else permissionPerObject = (exp => FieldAccessPredicate(silma, silpe)(position))
                    // YANNIS: todo: check: is this enough for backpointer access here?
          }

            // return a universal quantification on all sequence elements
         val boundedId = nameGenerator.createIdentifier("i")
         Forall(List(ListVarDecl(boundedId)), List(), permissionPerObject(SeqIndex(silseq, LocalVar(boundedId))))(
           position)
     /*
           // YANNIS: todo: this case
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
      }  */

        // unfolding
      case unfolding@chalice.Unfolding(predicateAccess, body) =>
        val silpa = translateExp(predicateAccess, myThis, pTrans)
        val silbody = translateExp(body, myThis, pTrans)
        Unfolding(silpa.asInstanceOf[PredicateAccessPredicate], silbody)(position)

        // eval is not supported
      case e: chalice.Eval => messages += GeneralEvalNotImplemented(e)

       // quantification and aggregation
      case e@chalice.Quantification(q, is, body) =>
        var boundedVars: List[LocalVarDecl] = TrueLit
        var boundingExpression: Exp = null
        if (e.isInstanceOf[chalice.TypeQuantification])
          boundedVars = is map { i => LocalVarDecl(i, translateType(e.asInstanceOf[chalice.TypeQuantification].t)) }
        else if (e.isInstanceOf[chalice.SeqQuantification]) {
          val seq = e.asInstanceOf[chalice.SeqQuantification].seq
          val silseq = translateExp(seq, myThis, pTrans)
          val tp = translateType(seq.typ.asInstanceOf[chalice.SeqClass].parameter)
          boundedVars = is map { i =>
            boundingExpression = And(boundingExpression, SeqContains(LocalVar(i), silseq))
            LocalVarDecl(i, SeqType(tp))
          }
        }
        else {
          // YANNIS: todo: fix set implementation in SIL
          val set =  e.asInstanceOf[chalice.SetQuantification].set
          val tp = translateType(set.typ.asInstanceOf[chalice.SetClass].parameter)
          val silset = translateExp(set, myThis, pTrans)
          boundedVars = is map { i =>
            boundingExpression = And(boundingExpression, SetContains(LocalVar(i), silset))
            LocalVarDecl(i, SetType(tp))
          }
        }
        val silbody = translateExp(body, myThis, pTrans)
        q match {
          case chalice.Forall => Forall(boundedVars, Seq(), Implies(boundingExpression, silbody))(position)
          case chalice.Exists =>  Exists(boundedVars, And(boundingExpression, silbody))(position)
          case _ => messages += AggregatesNotImplemented(e) ; null  // YANNIS: todo
        }

      case chalice.BoolLiteral(b) => if(b) TrueLit()(position) else FalseLit()(position)
      case chalice.IntLiteral(n) => IntLit(n)(position)
      case chalice.NullLiteral() => NullLit()(position)
      case chalice.StringLiteral(s) => null // YANNIS: todo: SIL does not support strings
      case chalice.VariableExpr(v) => LocalVar(v)(position)
      case chalice.LockBottomLiteral() => null // YANNIS: todo: support later
      case chalice.MaxLockLiteral() => null // YANNIS: todo support later
      case chalice.ImplicitThisExpr() => (myThis.localVar)(position)
      case chalice.ExplicitThisExpr() => (myThis.localVar)(position)
      case chalice.VariableExpr(v) => LocalVar(v)(position)
     }
  }

  protected def translateBody(cMethod: chalice.Method, myThis: LocalVarDecl, pTrans: PermissionTranslator) = {
    val sMethod = symbolMap(cMethod).asInstanceOf[Method]
    sMethod.body = Seqn(cMethod.body.map(translateStm(_, myThis, pTrans)))()
  }

  protected def translateStm(cStm: chalice.Statement, myThis: LocalVarDecl, pTrans: PermissionTranslator) : Stmt = {
    val position = new SourcePosition(cStm.pos.line, cExp.pos.column)
    st match {
      case chalice.Assert(e) => Assert(translateExp(e, myThis, pTrans))(position)
      case chalice.Assume(e) => Inhale(translateExp(e, myThis, pTrans))(position)
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
}

abstract class PermissionTranslator{
  def getK : Exp  // this is the permission expression that corresponds to rd(o.f)
    // getK is different depending on whether we are in a method, predicate or invariant context
    // in function contexts there is no such value

  // YANNIS: todo: add "this" parameter here and in clients; fix all occurrences of translateExp
  def apply(perm: chalice.Permission) = {
    perm match {
      case chalice.Full => FullPerm() // 100% permission
      case chalice.Frac(n) => FractionalPerm(translateExp(n, this), new IntLit(100)) // n% permission
      case chalice.Star => WildcardPerm() // rd* permission
      case chalice.Epsilon => getK // represents the permission given by the Chalice expression rd(o.f)
        // attention: chalice.Epsilon is a misnomer!!
      case chalice.MethodEpsilon => getK

      // for predicate and monitor k permissions, we use the global k value
      // the following two cases should only appear in the corresponding contexts
      case chalice.PredicateEpsilon => getK
      case chalice.MonitorEpsilon => getK
      // YANNIS: todo: deal with: case class ForkEpsilon(token: Expression) extends Write

      // counting permissions
      case chalice.Epsilons(n) => PermIntMul(translateExp(n, this), new EpsilonPerm()) // n*ε permissions
        // YANNIS: todo: attention: EpsilonPerm may be removed in future versions of SIL!

      // operations on permissions
      case chalice.PermTimesOp(lhs, rhs) => PermMul(translateExp(lhs, this), translateExp(rhs, this))
        // multiplication of two fractional permissions
      case chalice.IntPermTimes(n, p) => PermIntMul(translateExp(n, this), translateExp(p, this))
        // multiplication of an integer and a permission
      case chalice.PermTimesPlus(lhs, rhs) =>
        new PermAdd(translateExp(lhs, this), translateExp(rhs, this)) // p1+p2
      case chalice.PermTimesMinus(lhs, rhs) =>
        new PermSub(translateExp(lhs, this), translateExp(rhs, this)) // p1-p2
    }
  }
}

// permission translation in method contexts: getK is a formal parameter of the method
case class MethodPermissionTranslator(val KVar: LocalVarDecl) extends PermissionTranslator {
  override def getK = KVar.localVar
}

// permission translation in predicate and invariant contexts: getK is the global K defined for these contexts
case class PredicatePermissionTranslator(val globalK: DomainFunc) extends PermissionTranslator {
  override def getK = DomainFuncAppl(globalK, Seq(), Seq())
}

// in functions, all permissions must be translated to starred read permission
case class FunctionPermissionTranslator() extends PermissionTranslator {
  override val getK = null

  override def apply(perm: chalice.Permission) = WildcardPerm()
}