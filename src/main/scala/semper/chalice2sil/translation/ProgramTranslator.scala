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

// todo: run and test program functionality
// later add the following functionality: deadlock avoidance, set/seq support, fix epsilon permissions,
// error-message and obsolete classes refactoring, aggregates support, channels support

class ProgramTranslator(val programOptions: semper.chalice2sil.ProgramOptions, val programName: String)
{
  // output of the translator
  val messages = scala.collection.mutable.ListBuffer[semper.chalice2sil.Message]()
    // messages generated in the translation
    // to be eliminated in future refactoring
  val silEnvironment = new SILProgramEnvironment()
    // contains all SIL members and local variables generated in the translation
    // to be eliminated in future refactoring

  // maps tokens to the corresponding forked Chalice method
    // at most one method fork per token is allowed within a method
  val joinTokens = new scala.collection.mutable.HashMap[(Method, String), JoinableInfo]()

  // translated invariants
  val silTranslatedInvariants = new scala.collection.mutable.HashMap[chalice.Class, Predicate]()

  // translated symbols
  val symbolMap = new scala.collection.mutable.HashMap[chalice.ASTNode, Node]()
    // maps Chalice class members and local variables to SIL members and local variables

  // name generator -- ensures uniqueness and validity of names of identifiers
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

    // translate monitorInvariant into a SIL predicate
    val pName = nameGenerator.createIdentifier(classNode.FullName + "_MonitorInvariant")
    val monitorPredicate = Predicate(pName, ths, monitorInvariant)()
    silEnvironment.silPredicates += (pName -> monitorPredicate)
    silTranslatedInvariants(classNode) = monitorPredicate
  }

  protected def translateType(cType: chalice.Type) : Type = {
    cType.id match {
      case "seq" =>
        // YANNIS: todo
 /*       if(typ.params.length != 1) { messages += WrongNumberOfTypeParameters; Int }
        val tvm = Map[TypeVar, Type]()
        val silT = translateType(cType.params.head)
        tvm += (typeVar, silT)
        new DomainType(seqDomain, tvm)*/
        null
      case "set" =>
        // YANNIS: todo
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

  // YANNIS: todo: revisit preconditions everywhere e.g. this!=null etc.

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

      // chalice2sil ignores all deadlock prevention specs;
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
          translateExp(cond, myThis, pTrans), translateExp(thn, myThis, pTrans), translateExp(els, myThis, pTrans)
        )(position)

      // arithmetic operators and set union, subtraction, intersection
      case chalice.Plus(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) Add(l, r)(position)
        else { // if lhs is not int, this translates to set union
/*          var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.union, List(l, r), t)*/ null // sets are not supported
        }
      case chalice.Minus(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) Sub(l, r)(position)
        else { // if lhs is not int, this translates to set subtraction
          /*var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.subtraction, List(l, r), t)*/  null // sets are not supported
        }
      case chalice.Times(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) Mul(l, r)(position)
        else { // if lhs is not int, this translates to set intersection
          /*var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.intersection, List(l, r), t)*/ null // sets are not supported
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
      case chalice.Less(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) LtCmp(l, r)(position)
        else { // if lhs is not int, this translates to set proper inclusion
          /*var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.subset, List(l, r), t)*/ null // sets are not supported
        }
      case chalice.AtMost(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) LeCmp(l, r)(position)
        else { // if lhs is not int, this translates to set proper inclusion
          /*var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.subsetEq, List(l, r), t)*/  null // sets are not supported
        }
      case chalice.AtLeast(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) GeCmp(l, r)(position)
        else { // if lhs is not int, this translates to set proper inclusion
          /*var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.supsetEq, List(l, r), t)*/  null // sets are not supported
        }
      case chalice.Greater(lhs, rhs) =>
        val l = translateExp(lhs, myThis, pTrans)
        val r = translateExp(rhs, myThis, pTrans)
        if(lhs.typ == chalice.IntClass) GtCmp(l, r)(position)
        else { // if lhs is not int, this translates to set proper inclusion
          /*var t = l.typVarsMap.getOrElse(SetDomain.typeVar, null)
          if(t == null) { messages += TypeError(l) ; t = Int }
          new DomainFuncApp(SetDomain.supset, List(l, r), t)*/  null // sets are not supported
        }

      // sequence operators
      case chalice.EmptySeq(t) => EmptySeq(translateType(t))(position)
      case chalice.ExplicitSeq(elems) => ExplicitSeq(elems.map(translateExp(_, myThis, pTrans)))(position)
      case chalice.Range(lhs, rhs) =>
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

      // remaining set operators: not supported

      // member access
      case _:chalice.ThisExpr => myThis.localVar
      case ma@chalice.MemberAccess(e, id) =>
        val cls = e.typ
        if(cls!=null)
        {
          cls.LookupMember(id) match {
            case None => messages += TypeError() ; null
            case Some(f) =>
              val sf = symbolMap.getOrElse(f, null)
              if (sf == null) { messages += TypeError() ; null }
              else {
                if(f.isInstanceOf[chalice.Field])
                  FieldAccess(translateExp(e, myThis, pTrans), sf.asInstanceOf[Field])(position)
                else PredicateAccess(translateExp(e, myThis, pTrans), sf.asInstanceOf[Predicate])(position)
              }
          }
        }
        else { messages += TypeError() ; null }
        // YANNIS: todo: case class BackPointerMemberAccess(ex: Expression, typeId: String, fieldId: String) extends Expression {}

      // access permissions
      case chalice.Access(ma, perm) =>
        val silma = translateExp(ma, myThis, pTrans)
        val silpe = pTrans(perm, myThis)
        if (ma.isPredicate) PredicateAccessPredicate(silma.asInstanceOf[PredicateAccess], silpe)(position)
        else FieldAccessPredicate(silma.asInstanceOf[FieldAccess], silpe)(position)
      case chalice.BackPointerAccess(ma, perm) =>
        val silma = translateExp(ma, myThis, pTrans)
        val silpe = pTrans(perm, myThis)
        FieldAccessPredicate(silma.asInstanceOf[FieldAccess], silpe)(position)

      case chalice.AccessAll(obj, perm) =>
        val silo = translateExp(obj, myThis, pTrans)
        val silpe = pTrans(perm, myThis)
        var silexp : Exp = TrueLit()()

        if(obj==null || obj.typ == null) messages += UnknownAstNode(obj)
        else {
            // add access to all declared fields and predicates
          obj.typ.DeclaredFields.foreach{ f =>
            val sf = symbolMap.getOrElse(f, null)
            if (sf == null) messages += TypeError()
            else silexp = And(silexp, FieldAccessPredicate(FieldAccess(silo, sf.asInstanceOf[Field])(), silpe)())()
          }

           // YANNIS: todo: does this cover predicates or only fields?

           // YANNIS: todo: add access to backpointer fields

         }

         // return the complete access permission expression
         silexp

      case chalice.AccessSeq(seq, member, perm) => // YANNIS: todo: refactor this case
          val silseq = translateExp(seq, myThis, pTrans)
          val silpe = pTrans(perm, myThis)
          var permissionPerObject : Exp => Unit = null
          var silexp: Exp = TrueLit()()
            // this closure will return the permission expression for each element of the sequence

          // define permissionPerObject
          member match {
            case None =>
              if(seq==null || seq.typ==null) messages += UnknownAstNode(seq) // YANNIS: todo: other health conditions
              else {
                  permissionPerObject = (exp => {
                    seq.typ.parameters(0).DeclaredFields.foreach{ f =>
                        val sf = symbolMap.getOrElse(f, null)
                        if (sf == null) messages += TypeError()
                        else
                          silexp = And(silexp, FieldAccessPredicate(FieldAccess(exp, sf.asInstanceOf[Field])(), silpe)())() }
                    // YANNIS: todo: fix.  the above code includes Chalice predicates, which should be coded as
                       // PredicateAccessPredicate objects
                    // YANNIS: todo: add access to backpointer fields
                  })
               }
            case Some(f) =>
              if(f.isPredicate) permissionPerObject =
                (exp => { silexp = PredicateAccessPredicate(exp.asInstanceOf[PredicateAccess], silpe)(position)})
              else permissionPerObject =
                (exp => silexp = FieldAccessPredicate(silseq.asInstanceOf[FieldAccess], silpe)(position))
           }

                    // YANNIS: todo: check: is this enough for backpointer access here?

         // return a universal quantification on all sequence elements
         val boundedId = nameGenerator.createIdentifier("i")
         permissionPerObject(SeqIndex(silseq, LocalVar(boundedId)(Int))())
          // YANNIS: todo: ensure that (in exceptional conditions) the flow does not reach this point
          // YANNIS: todo: bound the index of the array in the quantification
         Forall(List(LocalVarDecl(boundedId, Int)()), List(), silexp)(position)
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
      case e: chalice.Eval => messages += GeneralEvalNotImplemented(e) ; null

       // quantification and aggregation
        // YANNIS: todo: bounded identifiers must be constructed with the name generator too
      case e: chalice.Quantification =>
        var boundedVars : List[LocalVarDecl] = null
        var boundingExpression: Exp = null
        if (e.isInstanceOf[chalice.TypeQuantification])
          boundedVars = e.Is map { i => LocalVarDecl(i, translateType(e.asInstanceOf[chalice.TypeQuantification].t))() }
        else if (e.isInstanceOf[chalice.SeqQuantification]) {
          val seq = e.asInstanceOf[chalice.SeqQuantification].seq
          val silseq = translateExp(seq, myThis, pTrans)
          val tp = translateType(new chalice.Type(seq.typ.asInstanceOf[chalice.SeqClass].parameter))
          boundedVars = e.Is map { i =>
            boundingExpression = And(boundingExpression, SeqContains(LocalVar(i)(tp), silseq)())()
            LocalVarDecl(i, tp)()
          }
        }
        else {
          val set =  e.asInstanceOf[chalice.SetQuantification].dom
          val tp = translateType(new chalice.Type(set.typ.asInstanceOf[chalice.SetClass].parameter))
          val silset = translateExp(set, myThis, pTrans)
          boundedVars = e.Is map { i =>
            boundingExpression = And(boundingExpression, SeqContains /*YANNIS: todo: SetContains*/ (LocalVar(i)(tp), silset)())()
            LocalVarDecl(i, tp)()
          }
        }
        val silbody = translateExp(e.E, myThis, pTrans)  // e.E refers to the body of the quantification
        e.Q match {
          case chalice.Forall => Forall(boundedVars, Seq(), Implies(boundingExpression, silbody)())(position)
          case chalice.Exists =>  Exists(boundedVars, And(boundingExpression, silbody)())(position)
          case _ => messages += UnknownAstNode(e) ; null
        }

      case chalice.BoolLiteral(b) => if(b) TrueLit()(position) else FalseLit()(position)
      case chalice.IntLiteral(n) => IntLit(n)(position)
      case chalice.NullLiteral() => NullLit()(position)
      case chalice.StringLiteral(s) => null // YANNIS: todo: SIL does not support strings
      case chalice.LockBottomLiteral() => null
      case chalice.MaxLockLiteral() => null
      case chalice.ImplicitThisExpr() => myThis.localVar
      case chalice.ExplicitThisExpr() => myThis.localVar
      case chalice.VariableExpr(v) => LocalVar(v)(Int/*YANNIS: todo: fix*/)
     }
  }

  protected def translateBody(cMethod: chalice.Method, myThis: LocalVarDecl, pTrans: PermissionTranslator) = {
    val sMethod = symbolMap(cMethod).asInstanceOf[Method]
    sMethod.locals = Seq()
    sMethod.body = Seqn(cMethod.body.map(translateStm(_, myThis, pTrans, sMethod)))()
  }

  protected def translateStm(cStm: chalice.Statement, myThis: LocalVarDecl,
                             pTrans: PermissionTranslator, silMethod: Method) : Stmt = {
    val position = new SourcePosition(null, cStm.pos.line, cStm.pos.column)
    cStm match {
      case chalice.Assert(e) => Assert(translateExp(e, myThis, pTrans))(position)
      case chalice.Assume(e) => Inhale(translateExp(e, myThis, pTrans))(position)
      case chalice.BlockStmt(ss) => Seqn(ss.map(translateStm(_, myThis, pTrans, silMethod)))(position)
      case chalice.IfStmt(guard, chalice.BlockStmt(thn), els) =>
        If(translateExp(guard, myThis, pTrans), Seqn(thn.map(translateStm(_, myThis, pTrans, silMethod)))(),
          els match {
            case None => Seqn(Seq())()
            case Some(s) => translateStm(s, myThis, pTrans, silMethod)
          }
        )(position)
      case w@chalice.WhileStmt(guard, _, _, _ /*YANNIS: todo: lockchange not supported*/, body) =>
        // YANNIS: todo: what is the difference between newInvs and oldInvs?
        While(translateExp(guard, myThis, pTrans), w.Invs.map(translateExp(_, myThis, pTrans)), Seq(),
          translateStm(body, myThis, pTrans, silMethod)
        )(position)
      case chalice.Assign(v@chalice.VariableExpr(name), rhs) =>
        LocalVarAssign(LocalVar(name)(Int /*YANNIS: todo: fix*/),
          translateExp(rhs.asInstanceOf[chalice.Expression], myThis, pTrans))(position)
          // YANNIS: todo: RValue is richer than Expression in Chalice, but this is not supported here!
      case chalice.FieldUpdate(ma, rhs) =>
        val silma = translateExp(ma, myThis, pTrans)
        if (!silma.isInstanceOf[FieldAccess]) { messages += TypeError(); null }
        else FieldAssign(silma.asInstanceOf[FieldAccess],
          translateExp(rhs.asInstanceOf[chalice.Expression], myThis, pTrans))()
          // YANNIS: todo: RValue is richer than Expression in Chalice, but this is not supported here!
      case chalice.LocalVar(v, rhs) => // YANNIS: todo
        silMethod.locals = silMethod.locals :+
          LocalVarDecl(v.id/*YANNIS: todo: name generators?*/, Int /*YANNIS: todo: fix*/)(position)
        rhs match {
          case None => Seqn(Seq())()
          case Some(e) =>
            LocalVarAssign(LocalVar(v.id)(Int /*YANNIS: todo: fix*/), translateExp(e.asInstanceOf[chalice.Expression],
              myThis, pTrans))(position)
        }

      case chalice.Call(_, lhs, target, methodName, args) =>
        // YANNIS: todo: implicit locals declaration
          // the first argument of Call is a mask that defines which variables of the lhs are implicitly defined
          // the feature is not yet implemented

        // spot Chalice method object
        val m = target.typ.LookupMember(methodName)

        // spot SIL method object
        val silMethod = m match {
          case None => messages += TypeError() ; return null
          case Some(cMethod) =>
            val s = symbolMap.getOrElse(cMethod, { messages += TypeError() ; return null })
            if(!s.isInstanceOf[Method]) { messages += TypeError() ; return null }
            s.asInstanceOf[Method]
        }

        // create fresh read permission
        val newK = LocalVarDecl(nameGenerator.createIdentifier("newK"), Perm)()

        // create a method call inside a fresh permission block
        FreshReadPerm(Seq(newK.localVar),
          MethodCall(silMethod,
               translateExp(target, myThis, pTrans) /*this*/
            :: newK.localVar /*permission*/
            :: args.map(translateExp(_, myThis, pTrans))/*arguments*/,
            lhs.map(x => LocalVar(x.id)(Int /*YANNIS: todo: fix type*/))/*targets*/
          )(position)
        )(position)

      case chalice.SpecStmt(_, _, _, _) => messages += TypeError() ; null // YANNIS: todo: SpecStatements not supported
      case chalice.Install(_, _, _) => messages += TypeError() ; null // YANNIS: todo: mu reordering not supported yet

      case chalice.Share(obj, _, _) => // YANNIS: todo: mu ordering and deadlock avoidance not supported yet
        // YANNIS: todo: assert that the object is not already shared

        // exhale the monitor invariant
        val monitorPredicate = silTranslatedInvariants.getOrElse(obj.typ, { messages += TypeError() ; return null })
        Exhale(PredicateAccess(translateExp(obj, myThis, pTrans), monitorPredicate)(position))(position)

      case chalice.Unshare(_) => messages += TypeError() ; Seqn(Seq())(position) // YANNIS: todo: unsharing not supported

      case chalice.Acquire(obj) => // YANNIS: todo: mu ordering and deadlock avoidance not supported yet
        // YANNIS: todo: assert that the object is shared, not already held, and the waitlevel is lower than its mu

        // inhale the monitor invariant
        val monitorPredicate = silTranslatedInvariants.getOrElse(obj.typ, { messages += TypeError() ; return null })
        Inhale(PredicateAccess(translateExp(obj, myThis, pTrans), monitorPredicate)(position))(position)

      case chalice.Release(obj) => // YANNIS: todo: mu ordering and deadlock avoidance not supported yet
        // YANNIS: todo: assert that the object is shared and held

        // exhale the monitor invariant
        val monitorPredicate = silTranslatedInvariants.getOrElse(obj.typ, { messages += TypeError() ; return null })
        Exhale(PredicateAccess(translateExp(obj, myThis, pTrans), monitorPredicate)(position))(position)

      // YANNIS: todo several unsupported features
      case chalice.RdAcquire(_) => messages += TypeError() ; null
      case chalice.RdRelease(_) => messages += TypeError() ; null
      case chalice.Lock(_, _, true) // this flag indicates "read" locks
        => messages += TypeError() ; null
      case chalice.Downgrade(_) => messages += TypeError() ; null
      case chalice.Free(_) => messages += TypeError() ; null

      case chalice.Lock(obj, block, false) =>
        val chalEquivalent = chalice.BlockStmt((chalice.Acquire(obj) :: block.ss) :+ chalice.Release(obj))
        translateStm(chalEquivalent, myThis, pTrans, silMethod)

      //forking
      case chalice.CallAsync(_, lhs, obj, id, args) =>
        // YANNIS: todo: implicit locals declaration

        // check if token has appeared in another fork (which is not supported)
        if (joinTokens.contains((silMethod, lhs.id))) { messages += TypeError() ; return null }

        // all generated statements are going here
        val statements = new mutable.MutableList[Stmt]()

        // assert that the token is not joinable
        // todo

        // spot chalice method and store it in joinTokens
        val m = obj.typ.LookupMember(id)
        val cMethod = m match {
          case None => messages += TypeError() ; return null
          case Some(cM) => cM
        }
        val joinableInfo = new JoinableInfo(cMethod.asInstanceOf[chalice.Method])
        joinTokens((silMethod, lhs.id)) = joinableInfo

        // ensure args.length is equal to the number of arguments expected by cMethod
        // todo

        // evaluate arguments and "old" expressions of the postcondition of cMethod and store them in local variables
        // also store the local variable names into joinableInfo.oldExpressions: first all arguments and then all "old"
        // expressions in order of appearance
        val argsAndOldIds = new mutable.MutableList[String]()
        for(i <- 0 until args.length) {
          val name = nameGenerator.createIdentifier("a" + i)
          argsAndOldIds += name
          val silLocalVariable = LocalVarDecl(name, Int /*todo:fix*/)(position)
          silMethod.locals = silMethod.locals :+ silLocalVariable
          statements += LocalVarAssign(silLocalVariable.localVar, translateExp(args(0), myThis, pTrans))(position)
        }
        val oldExpressions = Util.getOldExpressions(silMethod.posts)
        for(i <- 0 until oldExpressions.length) {
          val name = nameGenerator.createIdentifier("o" + i)
          argsAndOldIds += name
          val silLocalVariable = LocalVarDecl(name, Int /*todo:fix*/)(position)
          silMethod.locals = silMethod.locals :+ silLocalVariable
          statements += LocalVarAssign(silLocalVariable.localVar, oldExpressions(i))(position)
        }
        joinableInfo.argsAndOldIds = argsAndOldIds.toSeq

        // generate fresh permission variable
        // todo

        // spot SIL method; take its precondition and make argument substitutions (this and K inclusive)
        // store the result into sPre
        // todo

        // exhale sPre
        // todo

        // make token joinable
        // todo
        null
    }
    // YANNIS: todo: finish
    /*
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

  abstract class PermissionTranslator{
    def getK : Exp  // this is the permission expression that corresponds to rd(o.f)
    // getK is different depending on whether we are in a method, predicate or invariant context
    // in function contexts there is no such value

    def apply(perm: chalice.Permission, myThis: LocalVarDecl) = {
      perm match {
        case chalice.Full => FullPerm()() // 100% permission
        case chalice.Frac(n) => FractionalPerm(translateExp(n, myThis, this), new IntLit(100)())() // n% permission
        case chalice.Star => WildcardPerm()() // rd* permission
        case chalice.Epsilon => getK // represents the permission given by the Chalice expression rd(o.f)
        // attention: chalice.Epsilon is a misnomer!!
        case chalice.MethodEpsilon => getK

        // for predicate and monitor k permissions, we use the global k value
        // the following two cases should only appear in the corresponding contexts
        case chalice.PredicateEpsilon(_) => getK
        case chalice.MonitorEpsilon(_) => getK
        // YANNIS: todo: deal with: case class ForkEpsilon(token: Expression) extends Write

        // counting permissions
        case chalice.Epsilons(n) => IntPermMul(translateExp(n, myThis, this), EpsilonPerm()())() // n*ε permissions

        // operations on permissions
        case chalice.PermTimes(lhs, rhs) => PermMul(translateExp(lhs, myThis, this), translateExp(rhs, myThis, this))()
        // multiplication of two fractional permissions
        case chalice.IntPermTimes(n, p) => IntPermMul(translateExp(n, myThis, this), translateExp(p, myThis, this))()
        // multiplication of an integer and a permission
        case chalice.PermPlus(lhs, rhs) =>
          new PermAdd(translateExp(lhs, myThis, this), translateExp(rhs, myThis, this))() // p1+p2
        case chalice.PermMinus(lhs, rhs) =>
          new PermSub(translateExp(lhs, myThis, this), translateExp(rhs, myThis, this))() // p1-p2
      }
    }
  }

  // permission translation in method contexts: getK is a formal parameter of the method
  case class MethodPermissionTranslator(val KVar: LocalVarDecl) extends PermissionTranslator {
    override def getK = KVar.localVar
  }

  // permission translation in predicate and invariant contexts: getK is the global K defined for these contexts
  case class PredicatePermissionTranslator(val globalK: DomainFunc) extends PermissionTranslator {
    override def getK = DomainFuncApp(globalK, Seq(), new scala.collection.immutable.HashMap[TypeVar,Type]())()
  }

  // in functions, all permissions must be translated to starred read permission
  case class FunctionPermissionTranslator() extends PermissionTranslator {
    override val getK = null

    override def apply(perm: chalice.Permission, myThis: LocalVarDecl) = WildcardPerm()()
  }
}

object Util {
  // takes a SIL expression and returns the sequence of all "old" expressions in the order they appear in it
  // (the "old" node is stripped off from the result
  def getOldExpressions(sExp: Seq[Exp]) : Seq[Exp] = {
    val oldExpressions = new mutable.MutableList[Exp]()
    sExp.foreach((e:Exp) => semper.sil.ast.utility.Visitor.visit(e)(
      { case n:Node => { if(n.isInstanceOf[Old]) oldExpressions += n.asInstanceOf[Old].exp ; Unit } }
    ))
    oldExpressions
  }
}

class JoinableInfo(val cMethod: chalice.Method) {
  var argsAndOldIds: Seq[String] = null
}