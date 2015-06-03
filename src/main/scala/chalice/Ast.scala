//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

/* This file is based on the sources of Microsoft's Chalice tool, which is
 * hosted on Codeplex (http://chalice.codeplex.com/) and licensed under
 * Microsoft Public License (see LICENSE_Ms-PL.txt in the root directory).
 * The file might have changed since it has been forked, in particular, existing
 * code might have been modified or removed, and new code might have been added.
 */

package chalice

import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Positional
import scala.language.postfixOps

trait ASTNode extends Positional

/**
 * Classes and types
 */

sealed abstract class TopLevelDecl(val id: String) extends ASTNode

sealed class Class(val classId: String, val parameters: List[Class], val module: String, val members: List[Member]) extends TopLevelDecl(classId) {
  def IsInt: Boolean = false
  def IsBool: Boolean = false
  def IsRef: Boolean = true
  def IsNull: Boolean = false
  def IsString: Boolean = false
  def IsMu: Boolean = false
  def IsSet: Boolean = false
  def IsSeq: Boolean = false
  def IsToken: Boolean = false
  def IsChannel: Boolean = false
  def IsState: Boolean = false
  def IsNormalClass = true
  def IsPermission = false

  // TODO: this is an ugly hack because this class cannot be a case class at the moment. it would be better to improve the AST hierarchy such that this class can be a case class
  override def hashCode() = ((classId, parameters, module, members)).hashCode
  override def equals(other: Any) = other match {
    case o: Class => ((o.classId, o.parameters, o.module, o.members)).equals((classId, parameters, module, members))
    case _ => false
  }

  lazy val DeclaredFields = members flatMap {case x: Field => List(x); case _ => Nil}
  lazy val MentionableFields = Fields filter {x => ! x.Hidden}
  lazy val MonitorInvariants = members flatMap {case x: MonitorInvariant => List(x); case _ => Nil}
  lazy val Fields:List[Field] = DeclaredFields ++ (if (IsRefinement) refines.Fields else Nil)

  private lazy val id2member:Map[String,NamedMember] = Map() ++ {
    val named = members flatMap {case x: NamedMember => List(x); case _ => Nil};
    (named map {x => x.Id}) zip named
  }
  def LookupMember(id: String): Option[NamedMember] = {
    if (id2member contains id)
      Some(id2member(id))
    else if (IsRefinement)
      refines.LookupMember(id)
    else if (IsRef && this != RootClass) {
      // check with root class
      RootClass LookupMember id match {
        case Some(m) if (! m.Hidden) => Some(m)
        case _ => None
      }
    } else
      None
  }
  def FullName: String = if(parameters.isEmpty) id else id + "<" + parameters.tail.foldLeft(parameters.head.FullName){(a, b) => a + ", " + b.FullName} + ">"
  override def toString = FullName

  // Says whether or not to compile the class (compilation ignores external classes)
  var IsExternal = false

  // Refinement extension
  var IsRefinement = false
  var refinesId: String = null
  var refines: Class = null
  lazy val CouplingInvariants = members flatMap {case x: CouplingInvariant => List(x); case _ => Nil}
  lazy val Replaces: List[Field] = CouplingInvariants flatMap (_.fields)
}
object Class {
  def apply(classId: String, parameters: List[Class], module: String, members: List[Member]) = new Class(classId, parameters, module, members)
  def unapply(c: Class) = Some((c.classId, c.parameters, c.module, c.members))
}
sealed case class Channel(channelId: String, parameters: List[Variable], private val rawWhere: Expression) extends TopLevelDecl(channelId) {
  lazy val where: Expression = rawWhere.transform {
    case Epsilon | MethodEpsilon => Some(ChannelEpsilon(None))
    case _ => None
  }
}

sealed case class SetClass(parameter: Class) extends Class("set", List(parameter), "default", Nil) {
  override def IsRef = false
  override def IsSet = true
  override def IsNormalClass = false
}

sealed case class SeqClass(parameter: Class) extends Class("seq", List(parameter), "default", Nil) {
  override def IsRef = false;
  override def IsSeq = true;
  override def IsNormalClass = false
}
object PermClass extends Class("$Permission", Nil, "default", Nil) {
  override def IsRef = false
  override def IsPermission = true
  override def IsNormalClass = false
}
object IntClass extends Class("int", Nil, "default", Nil) {
  override def IsRef = false
  override def IsInt = true
  override def IsNormalClass = false
}
object BoolClass extends Class("bool", Nil, "default", Nil) {
  override def IsRef = false
  override def IsBool = true
  override def IsNormalClass = false
}
object NullClass extends Class("null", Nil, "default", Nil) {
  override def IsNull = true
  override def IsNormalClass = false
}
object StringClass extends Class("string", Nil, "default", Nil) {
  override def IsRef = false
  override def IsString = true
  override def IsNormalClass = false
}
object MuClass extends Class("$Mu", Nil, "default", Nil) {
  override def IsRef = false
  override def IsMu = true
  override def IsNormalClass = false
}
case class TokenClass(c: Type, m: String) extends Class("token", Nil, "default", List(
  new SpecialField("joinable", new Type(BoolClass), false)
))
{
  var method = null: Method
  override def IsRef = true
  override def IsToken = true
  override def IsNormalClass = false
  override def FullName: String = "token<" + c.FullName + "." + m + ">"
}
case class ChannelClass(ch: Channel) extends Class(ch.id, Nil, "default", Nil) {
  override def IsRef = true
  override def IsChannel = true
  override def IsNormalClass = false
}

object RootClass extends Class("$root", Nil, "default", List(
  new SpecialField("mu", new Type(MuClass), false),
  new SpecialField("held", new Type(BoolClass), true),
  new SpecialField("rdheld", new Type(BoolClass), true)
  ))  // joinable and held are bool in Chalice, but translated into an int in Boogie

sealed class Type(val id: String, val params: List[Type]) extends ASTNode {  // denotes the use of a type
  var typ: Class = null
  def this(id: String) = { this(id, Nil); }
  def this(cl: Class) = { this(cl.id); typ = cl }
  def FullName: String = if(params.isEmpty) id else id + "<" + params.tail.foldLeft(params.head.FullName){(a, b) => a + ", " + b.FullName} + ">"

  // TODO: this is an ugly hack because this class cannot be a case class at the moment. it would be better to improve the AST hierarchy such that this class can be a case class
  override def hashCode() = ((id, params)).hashCode
  override def equals(other: Any) = other match {
    case o: Type => ((o.id, o.params)).equals((id, params))
    case _ => false
  }
}
object Type {
  def apply(id: String, params: List[Type]) = new Type(id, params)
  def unapply(t: Type): Option[(String, List[Type])] = Some((t.id, t.params))
}
sealed case class TokenType(C: Type, m: String) extends Type("token", Nil) {  // denotes the use of a type
  typ = TokenClass(C, m);
  var method = null : Method;

  override def FullName: String = "token<" + C.FullName + "." + m + ">"
}

/**
 * Members
 */

sealed abstract class Member extends ASTNode {
  val Hidden: Boolean = false  // hidden means not mentionable in source
}
case class MonitorInvariant(private val rawE: Expression) extends Member {
  lazy val e: Expression = rawE.transform {
    case Epsilon | MethodEpsilon => Some(MonitorEpsilon(None))
    case _ => None
  }
}

sealed abstract class NamedMember(id: String) extends Member {
  val Id = id
  var Parent: Class = null
  def FullName = Parent.id + "." + Id
}
sealed class Field(val id: String, val typ: Type, val isGhost: Boolean, val isTracked: Boolean = false) extends NamedMember(id)
case class SpecialField(name: String, tp: Type, hidden: Boolean) extends Field(name, tp, false, false) {  // direct assignments are not allowed to a SpecialField
  override def FullName = id
  override val Hidden = hidden

  // TODO: this is an ugly hack because this class cannot be a case class at the moment. it would be better to improve the AST hierarchy such that this class can be a case class
  override def hashCode() = ((id, typ, isGhost)).hashCode
  override def equals(other: Any) = other match {
    case o: Field => ((o.id, o.typ, o.isGhost)).equals((id, typ, isGhost))
    case _ => false
  }
}
object Field {
  def apply(id: String, typ: Type, isGhost: Boolean, isTracked: Boolean) = new Field(id, typ, isGhost, isTracked)
  def unapply(f: Field): Option[(String, Type, Boolean, Boolean)] = Some((f.id, f.typ, f.isGhost, f.isTracked))
}
sealed abstract class Callable(id: String) extends NamedMember(id) {
  def Spec:List[Specification]
  def Body:List[Statement]
  def Ins:List[Variable]
  def Outs:List[Variable]
}
case class Method(id: String, ins: List[Variable], outs: List[Variable], spec: List[Specification], body: List[Statement]) extends Callable(id) {
  override def Spec = spec
  override def Body = body
  override def Ins = ins
  override def Outs = outs
}
case class Predicate(id: String, private val rawDefinition: Expression) extends NamedMember(id) {
  lazy val definition: Expression = rawDefinition.transform {
    case Epsilon | MethodEpsilon => Some(PredicateEpsilon(None))
    case _ => None
  }
}
case class Function(id: String, ins: List[Variable], out: Type, spec: List[Specification], definition: Option[Expression]) extends NamedMember(id) {
  // list of predicates that this function possibly depends on (that is, predicates
  // that are mentioned in the functions precondition)
  def dependentPredicates: Set[MemberAccess] = {
    var predicates: Set[MemberAccess] = Set()
    spec foreach {
      case Precondition(e) =>
        e visit {_ match {
          case pred@MemberAccess(e, p) if pred.isPredicate =>
            predicates += pred
          case _ =>}
        }
      case _ =>
    }
    predicates
  }
  // list of predicates that this function depends on, and that can be used to trigger the definitional axiom.
  // currently, a predicate has to fulfill the following criteria:
  // 1. the predicate must be mentioned in the precondition
  // 2. a) the predicate must be unfolded before any recursive function call (where recursive means another function
  //       with the same height
  //    b) the unfolding expression must unfold "the whole" permission to the predicate; that is, if the precondition
  //       contains 'acc(o.p, e)' for some permission 'e', thn the unfolding expression must also unfold 'e' of 'p'.
  // 3. the method precondition can only mention fixed permissions to the predicate. this includes a constant
  //    fraction or an abstract read permission (but not rd*). variables in permission expressions or other amounts
  //    are not allowed.
  // Note that this criterion could be relaxed for functions that depend on two predicates, and unfold both but in
  // two different branches and calls a (potentially different) recursive function in both branches.  For instance:
  //    f(b: bool) requires p && q; { b ? unfolding p in g() : unfolding q in f(true) }
  // In such cases, one could generate two definitional axioms, one for every branch, and have a trigger for p on one
  // axiom, and one for q on the other.  This can also be generalized to more branches and predicates.
  def dependentPredicatesForTriggers: Set[MemberAccess] = {
    var candidates: Set[Access] = Set()
    var nonTriggerPredicates: Set[MemberAccess] = Set()

    // inspect precondition
    spec foreach {
      case Precondition(e) =>
        e visitOpt {
          case acc @ Access(pred @ MemberAccess(e, f), perm) if pred.isPredicate =>
            perm match {
              case Full | MethodEpsilon | Frac(IntLiteral(_)) | Epsilons(IntLiteral(_)) =>
                candidates += acc
              case _ =>
                nonTriggerPredicates += pred
            }
            false // no further recursion necessary
          case pred @ MemberAccess(e, f) if pred.isPredicate =>
            candidates += Access(pred, Full)
            false // no further recursion necessary
          case _ => true
        }
      case _ =>
    }

    // filter out predicates for which forbidden predicates have been seen
    candidates = candidates filter {
      _ match {
        case Access(pred, perm) => !nonTriggerPredicates.contains(pred)
      }
    }

    // check that predicates are unfolded before recursive calls
    val allCandidates: Set[MemberAccess] = candidates map {
      case Access(pred, perm) => pred
    }
    // start from all predicate candidates from the precondition
    var unfoldedForEveryCall = allCandidates

    var currentlyUnfolded: List[MemberAccess] = List()
    definition match {
      case None => Nil
      case Some(definition) =>
        val before: RValue => Boolean = {
          case Unfolding(acc @ Access(pred @ MemberAccess(e, f), perm), in) if candidates.contains(acc) =>
            currentlyUnfolded ::= pred
            true
          case fa @ FunctionApplication(receiver, name, args) if height == fa.f.height =>
            // remove all predicates that are not unfolded at the time of the function call
            unfoldedForEveryCall = unfoldedForEveryCall -- (allCandidates -- currentlyUnfolded)
            false
          case _ =>
            true
        }
        def after(e: RValue): Unit = {
          e match {
            case Unfolding(acc @ Access(pred @ MemberAccess(e, f), perm), in) =>
              currentlyUnfolded = currentlyUnfolded filter (x => !x.eq(pred))
            case _ =>
          }
        }
        definition.visitOpt2(before, after)
        assert(currentlyUnfolded.size == 0)
    }
    unfoldedForEveryCall
  }

  def apply(rec: Expression, args: List[Expression]): FunctionApplication = {
    val result = FunctionApplication(rec, id, args);
    result.f = this;
    result
  }
  var isUnlimited = false
  var isStatic = false
  var isRecursive = false
  var SCC: List[Function] = Nil
  // the 'height' of this function is determined by a topological sort of the
  // condensation of the call graph; mutually recursive functions get the same
  // height.
  var height: Int = -1
}
case class Condition(id: String, where: Option[Expression]) extends NamedMember(id)
sealed class Variable(val id: String, val t: Type, val isGhost: Boolean, val isImmutable: Boolean) extends ASTNode {
  val UniqueName = {
    val n = S_Variable.VariableCount
    S_Variable.VariableCount = S_Variable.VariableCount + 1
    id + "#" + n
  }
  val Id = id;
  def this(name: String, typ: Type) = this(name,typ,false,false);
  override def toString = (if (isGhost) "ghost " else "") + (if (isImmutable) "const " else "var ") + id;

  // TODO: this is an ugly hack because this class cannot be a case class at the moment. it would be better to improve the AST hierarchy such that this class can be a case class
  override def hashCode() = ((id, t, isGhost, isImmutable)).hashCode
  override def equals(other: Any) = other match {
    case o: Variable => ((o.id, o.t, o.isGhost, isImmutable)).equals((id, t, isGhost, isImmutable))
    case _ => false
  }
}
object Variable {
  def apply(id: String, t: Type, isGhost: Boolean, isImmutable: Boolean) = new Variable(id, t, isGhost, isImmutable)
  def unapply(v: Variable): Option[(String, Type, Boolean, Boolean)] = Some((v.id, v.t, v.isGhost, v.isImmutable))
}
object S_Variable { var VariableCount = 0 }
case class SpecialVariable(name: String, typ: Type) extends Variable(name, typ, false, false) {
  override val UniqueName = name
}
sealed abstract class Specification extends ASTNode
case class Precondition(e: Expression) extends Specification
case class Postcondition(e: Expression) extends Specification
case class LockChange(ee: List[Expression]) extends Specification

/**
 * Refinement members
 */

case class CouplingInvariant(ids: List[String], e: Expression) extends Member {
  assert(ids.size > 0)
  var fields: List[Field] = Nil
  /* Distribute 100 between fields */
  def fraction(field: Field): Permission = {
    val k = fields.indexOf(field)
    assert (0 <= k && k < fields.size)
    val part: Int = 100 / fields.size
    if (k == fields.size - 1)
      Frac(IntLiteral(100 - part * k))
    else
      Frac(IntLiteral(part)          )
  }
}
case class MethodTransform(id: String, ins: List[Variable], outs: List[Variable], spec: List[Specification], trans: Transform) extends Callable(id) {
  var refines = null: Callable
  var body = null:List[Statement]
  def Spec = {assert(refines != null); refines.Spec ++ spec}
  def Body = {
    assert(body != null);
    // make sure the body appears as if it is from a normal method
    def concretize(ss: List[Statement]): List[Statement] = ss flatMap {
      case r @ RefinementBlock(con, abs) =>
        con :::
        (for ((a,c) <- (r.during._1 zip r.during._2)) yield LocalVar(a, Some(new VariableExpr(c))))
      case BlockStmt(ss) => List(BlockStmt(concretize(ss)))
      case IfStmt(guard, BlockStmt(thn), None) => List(IfStmt(guard, BlockStmt(concretize(thn)), None))
      case IfStmt(guard, BlockStmt(thn), Some(els)) => List(IfStmt(guard, BlockStmt(concretize(thn)), Some(BlockStmt(concretize(List(els))))))
      case WhileStmt(guard, oi, ni, lks, BlockStmt(ss)) => List(WhileStmt(guard, oi ++ ni, Nil, lks, BlockStmt(concretize(ss))))
      case s => List(s)
    }
    concretize(body)
  }
  def Ins = {assert(refines != null); refines.Ins}
  def Outs = {assert(refines != null); refines.Outs ++ outs.drop(refines.Outs.size)}
}

sealed abstract class Transform extends ASTNode
/** Pattern matching within a block (zero or more) over deterministic statements */
case class BlockPat() extends Transform {
  def matches(s: Statement) = s match {
    case _:Assert => true
    case _:Assume => true
    case _:Assign => true
    case _:FieldUpdate => true
    case _:LocalVar => true
    case _ => false
  }
}
/** Matches any block of code (greedily) and acts as identity */
case class SkipPat() extends Transform
/** Replacement pattern for arbitrary block */
case class ProgramPat(code: List[Statement]) extends Transform {
  if (code.size > 0) pos = code.head.pos
}
case class IfPat(thn: Transform, els: Option[Transform]) extends Transform
case class WhilePat(invs: List[Expression], body: Transform) extends Transform
case class NonDetPat(is: List[String], code: List[Statement]) extends Transform {
  def matches(s: Statement) = s match {
    case _:Call => true
    case _:SpecStmt => true
    case _ => false
  }
}
case class InsertPat(code: List[Statement]) extends Transform
case class SeqPat(pats: List[Transform]) extends Transform {
  assert(pats.size > 0)
  pos = pats.head.pos;
}
case class RefinementBlock(con: List[Statement], abs: List[Statement]) extends Statement {
  if (con.size > 0) pos = con.head.pos
  // local variables in context at the beginning of the block
  var before: List[Variable] = null
  // shared declared local variables (mapping between abstract and concrete)
  lazy val during: (List[Variable], List[Variable]) = {
    val a = for (v <- abs.flatMap(s => s.Declares)) yield v;
    val c = for (v <- a) yield con.flatMap(s => s.Declares).find(_ == v).get
    (a,c)
  }
  override def Declares = con flatMap {_.Declares}
  override def Targets = (con ++ abs :\ Set[Variable]()) { (s, vars) => vars ++ s.Targets}
}

/**
 * Statements
 */

sealed abstract class Statement extends ASTNode {
  def Declares: List[Variable] = Nil // call after resolution
  def Targets: Set[Variable] = Set() // assigned local variables
}
case class Assert(e: Expression) extends Statement {
  var smokeErrorNr: Option[Int] = None
}
case class Assume(e: Expression) extends Statement
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

/**
 * Expressions
 */

sealed abstract class RValue extends ASTNode {
  var typ: Class = null
}
case class NewRhs(id: String, initialization: List[Init], lowerBounds: List[Expression], upperBounds: List[Expression]) extends RValue
case class Init(id: String, e: Expression) extends ASTNode {
  var f: Field = null;
}
sealed abstract class Expression extends RValue {
  def transform(f: Expression => Option[Expression]) = AST.transform(this, f)
  def visit(f: RValue => Unit) = AST.visit(this, f)
  def visitOpt(f: RValue => Boolean) = AST.visitOpt(this, f)
  def visitOpt2(f: RValue => Boolean, f2: RValue => Unit) = AST.visitOpt2(this, f, f2)
}
sealed abstract class Literal extends Expression
case class IntLiteral(n: Int) extends Literal
case class BoolLiteral(b: Boolean) extends Literal
case class NullLiteral() extends Literal
case class StringLiteral(s: String) extends Literal
case class MaxLockLiteral() extends Literal
case class LockBottomLiteral() extends Literal
case class VariableExpr(id: String) extends Expression {
  var v: Variable = null
  def this(vr: Variable) = { this(vr.id); v = vr; typ = vr.t.typ }
  def Resolve(vr: Variable) = { v = vr; typ = vr.t.typ }
}

case class Result() extends Expression
sealed abstract class ThisExpr extends Expression
case class ExplicitThisExpr() extends ThisExpr {
  override def hashCode = 0
  override def equals(other: Any) = other.isInstanceOf[ThisExpr]
}
case class ImplicitThisExpr() extends ThisExpr {
  override def hashCode = 0
  override def equals(other: Any) = other.isInstanceOf[ThisExpr]
}
case class MemberAccess(e: Expression, id: String) extends Expression {
  var isPredicate: Boolean = false
  var f: Field = null
  var predicate: Predicate = null
}
case class BackPointerMemberAccess(ex: Expression, typeId: String, fieldId: String) extends Expression

case class IfThenElse(con: Expression, thn: Expression, els: Expression) extends Expression

object PermissionType extends Enumeration {
  type PermissionType = Value
  val Fraction, Epsilons, Mixed = Value
}
import PermissionType._
sealed abstract class Permission extends Expression {
  typ = PermClass
  def permissionType: PermissionType
}
sealed abstract class Write extends Permission {
  override def permissionType = PermissionType.Fraction
}
object Full extends Write                // None
case class Frac(n: Expression) extends Write // Some(n)
sealed abstract class Read extends Permission {
  override def permissionType = PermissionType.Epsilons
}
object Epsilon extends Write                      // None
// we use Option for the argument of the next three classes as follows:
// the argument is Some(_) if the exression originates from the user (e.g. if he used acc(x,rd(monitor))),
// and None otherwise. If Some(_) is used, we have additional checks to ensure that we have read access
// to _ and _ is not null.
case class PredicateEpsilon(predicate: Option[Expression]) extends Write
case class MonitorEpsilon(monitor: Option[Expression]) extends Write
case class ChannelEpsilon(channel: Option[Expression]) extends Write
object MethodEpsilon extends Write
case class ForkEpsilon(token: Expression) extends Write
object Star extends Write               // Some(None)
case class Epsilons(n: Expression) extends Read   // Some(Some(n))

sealed abstract class ArithmeticPermission extends Permission
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


sealed abstract class PermissionExpr(perm: Permission) extends Expression
sealed abstract class WildCardPermission(perm: Permission) extends PermissionExpr(perm)
case class Access(ma: MemberAccess, var perm: Permission) extends PermissionExpr(perm)
case class BackPointerAccess(ma: BackPointerMemberAccess, var perm: Permission) extends PermissionExpr(perm)
case class AccessAll(obj: Expression, var perm: Permission) extends WildCardPermission(perm)
case class AccessSeq(s: Expression, f: Option[MemberAccess], var perm: Permission) extends WildCardPermission(perm)

case class Credit(e: Expression, n: Option[Expression]) extends Expression {
  val N = n match { case None => IntLiteral(1) case Some(n) => n }
}

case class Holds(e: Expression) extends Expression
case class RdHolds(e: Expression) extends Expression
case class Assigned(id: String) extends Expression {
  var v: Variable = null
}
case class Old(e: Expression) extends Expression
case class Not(e: Expression) extends Expression
case class FunctionApplication(obj: Expression, id: String, args: List[Expression]) extends Expression {
  var f: Function = null
}
case class Unfolding(pred: Access, in: Expression) extends Expression
sealed abstract class BinaryExpr(e0: Expression, e1: Expression) extends Expression {
  val E0 = e0
  val E1 = e1
  val ExpectedLhsType: Class = BoolClass  // sometimes undefined
  val ExpectedRhsType: Class = BoolClass  // sometimes undefined
  val ResultType: Class = BoolClass
  val OpName: String
  val mayBeSetOperation: Boolean = false // this is a patch to allow + - * and comparison to be overloaded for sets
}
case class Iff(e0: Expression, e1: Expression) extends BinaryExpr(e0,e1) {
  override val OpName = "<==>"
}
case class Implies(e0: Expression, e1: Expression) extends BinaryExpr(e0,e1) {
  override val OpName = "==>"
}
case class And(e0: Expression, e1: Expression) extends BinaryExpr(e0,e1) {
  override val OpName = "&&"
}
case class Or(e0: Expression, e1: Expression) extends BinaryExpr(e0,e1) {
  override val OpName = "||"
}
sealed abstract class ArithmeticExpr(e0: Expression, e1: Expression) extends BinaryExpr(e0,e1) {
  override val ExpectedLhsType = IntClass
  override val ExpectedRhsType = IntClass
  override val ResultType = IntClass
}
case class Plus(e0: Expression, e1: Expression) extends ArithmeticExpr(e0,e1) {
  override val OpName = "+"
  override val mayBeSetOperation = true  // may be set union
}
case class Minus(e0: Expression, e1: Expression) extends ArithmeticExpr(e0,e1) {
  override val OpName = "-"
  override val mayBeSetOperation = true  // may be set subtraction
}
case class Times(e0: Expression, e1: Expression) extends ArithmeticExpr(e0,e1) {
  override val OpName = "*"
  override val mayBeSetOperation = true  // may be set intersection
}
case class Div(e0: Expression, e1: Expression) extends ArithmeticExpr(e0,e1) {
  override val OpName = "/"
}
case class Mod(e0: Expression, e1: Expression) extends ArithmeticExpr(e0,e1) {
  override val OpName = "%"
}
sealed abstract class CompareExpr(e0: Expression, e1: Expression) extends BinaryExpr(e0,e1) {
  override val ExpectedLhsType = IntClass
  override val ExpectedRhsType = IntClass
  override val mayBeSetOperation = true  // may be set comparison
}
sealed abstract class EqualityCompareExpr(e0: Expression, e1: Expression) extends CompareExpr(e0,e1) {
  override val ExpectedLhsType = null;
  override val ExpectedRhsType = null;
}
case class Eq(e0: Expression, e1: Expression) extends EqualityCompareExpr(e0,e1) {
  override val OpName = "=="
}
case class Neq(e0: Expression, e1: Expression) extends EqualityCompareExpr(e0,e1) {
  override val OpName = "!="
}
case class Less(e0: Expression, e1: Expression) extends CompareExpr(e0,e1) {
  override val OpName = "<"
}
case class AtMost(e0: Expression, e1: Expression) extends CompareExpr(e0,e1) {
  override val OpName = "<="
}
case class AtLeast(e0: Expression, e1: Expression) extends CompareExpr(e0,e1) {
  override val OpName = ">="
}
case class Greater(e0: Expression, e1: Expression) extends CompareExpr(e0,e1) {
  override val OpName = ">"
}
case class LockBelow(e0: Expression, e1: Expression) extends CompareExpr(e0,e1) {
  override val ExpectedLhsType = null;
  override val ExpectedRhsType = null;
  override val OpName = "<<"
}

/**
 * Expressions: quantifiers and aggregates
 */

sealed trait Quant
object Forall extends Quant
object Exists extends Quant
object Sum extends Quant
object Max extends Quant

sealed abstract class Quantification(q: Quant, is: List[String], e: Expression) extends Expression {
  val Q = q;
  val Is = is;
  val E = e;
  var variables = null: List[Variable]; // resolved by type checker
}
case class SeqQuantification(q: Quant, is: List[String], seq: Expression, e: Expression) extends Quantification(q, is, e)

// The minmax field stores the minimum and maximum of a range if the TypeQuantification originates from
// a SeqQuantification (e.g. from "forall i in [0..2] :: ..". This is later needed in isDefined to
// assert that min <= max
case class TypeQuantification(q: Quant, is: List[String], t: Type, e: Expression, minmax: (Expression, Expression)) extends Quantification(q, is, e) {
  def this(q: Quant, is: List[String], t: Type, e: Expression) = this(q, is, t, e, null)
}

case class SetQuantification(q: Quant, is: List[String], dom: Expression, body: Expression) extends Quantification(q, is, body) {
  def this(q: Quant, is: List[String], dom: Expression) = this(q, is, dom, null)
}


/**
 * Expressions: sets
 */

case class EmptySet(t:Type) extends Literal
case class ExplicitSet(elems:List[Expression]) extends Expression

  // set bin. operators are overloaded arithmetic operators: + (union), - (difference), * (intersection)
  // set inclusion is overloaded sequence inclusion
  // set comparison operators are overloaded: ==, !=, and the sub/superset relations <, >, <=, >=

/**
 * Expressions: sequences
 */

case class EmptySeq(t: Type) extends Literal
case class ExplicitSeq(elems: List[Expression]) extends Expression
case class Range(min: Expression, max: Expression /* non-inclusive*/) extends Expression
case class Append(s0: Expression, s1: Expression) extends SeqAccess(s0, s1) {
  override val OpName = "++"
}
sealed abstract class SeqAccess(e0: Expression, e1: Expression) extends BinaryExpr(e0, e1) {
  override val ExpectedLhsType = null
  override val ExpectedRhsType = null
  override val ResultType = null
}
case class Length(e: Expression) extends Expression
case class At(s: Expression, n: Expression) extends SeqAccess(s, n) {
  override val OpName = ""
}
case class Drop(s: Expression, n: Expression) extends SeqAccess(s, n) {
  override val OpName = ""
}
case class Take(s: Expression, n: Expression) extends SeqAccess(s, n) {
  override val OpName = ""
}
case class Contains(n: Expression, s: Expression) extends SeqAccess(n, s) {
  override val OpName = "in"
  override val mayBeSetOperation = true  // may be set containment
}

// eval

case class Eval(h: EvalState, e: Expression) extends Expression
sealed abstract class EvalState {
  def target(): Expression;
}
case class AcquireState(obj: Expression) extends EvalState {
  def target() = obj
}
case class ReleaseState(obj: Expression) extends EvalState {
  def target() = obj
}
case class CallState(token: Expression, obj: Expression, id: String, args: List[Expression]) extends EvalState {
  var m = null: Method;
  def target() = token;
}

/**
 * AST operations
 */

object AST {
  /**
   * Flattens sequences of transforms and merges consecutive block patterns
   */
  def normalize(trans: Transform): Transform = trans match {
    case IfPat(thn, Some(els)) => IfPat(normalize(thn), Some(normalize(els)))
    case IfPat(thn, None) => IfPat(normalize(thn), None)
    case SeqPat(pats) =>
      val rec = pats flatMap {pat => normalize(pat) match {
        case SeqPat(pats) => pats;
        case x => List(x)
      }}
      def noTwoBlocks: List[Transform] => List[Transform] = {
        case BlockPat() :: (bp @ BlockPat()) :: l => noTwoBlocks(bp :: l)
        case x :: l => x :: noTwoBlocks(l)
        case Nil => Nil
      }
      SeqPat(noTwoBlocks(rec))
    case _ => trans
  }

  sealed abstract class TransformMatch
  case class Matched(ss: List[Statement]) extends TransformMatch {
    def this(s: Statement) = this(List(s))
  }
  case class Unmatched(t: Transform) extends TransformMatch

  /**
   * Matches a proper block to a transform.
   * Effects: some statements might be replaced by refinements blocks; Loops might have new invariants.
   * Requires: transform is normalized
   */
  def refine:(List[Statement], Transform) => TransformMatch = {
    // order is important!
    // reduction of base cases
    case (l, SeqPat(List(t))) => refine(l, t)
    case (List(BlockStmt(ss)), t) => refine(ss, t)
    // whole program
    case (l, ProgramPat(code)) => new Matched(RefinementBlock(code, l))
    case (l, SkipPat()) => Matched(l)
    // if pattern
    case (List(IfStmt(guard, thn, None)), t @ IfPat(thnT, None)) =>
      refine(thn.ss, thnT) match {
        case Matched(thn0) => new Matched(IfStmt(guard, BlockStmt(thn0), None))
        case _ => Unmatched(t)
      }
    case (List(IfStmt(guard, thn, Some(els))), t @ IfPat(thnT, Some(elsT))) =>
      (refine(thn.ss, thnT), refine(List(els), elsT)) match {
        case (Matched(thn0), Matched(els0)) => new Matched(IfStmt(guard, BlockStmt(thn0), Some(BlockStmt(els0))))
        case _ => Unmatched(t)
      }
    // while pattern
    case (List(WhileStmt(guard, oi, Nil, lks, body)), wp @ WhilePat(l, t)) =>
      refine(body.ss, t) match {
        case Matched(body0) => new Matched(WhileStmt(guard, oi, l, lks, BlockStmt(body0)))
        case _ => Unmatched(wp)
      }
    // non det pat
    case (l @ List(_: Call), NonDetPat(_, code)) => new Matched(RefinementBlock(code, l))
    case (l @ List(_: SpecStmt), NonDetPat(_, code)) => new Matched(RefinementBlock(code, l))
    // insert pat
    case (Nil, InsertPat(code)) => new Matched(RefinementBlock(code, Nil))
    // block pattern (greedy matching)
    case (l, bp @ BlockPat()) if (l forall {s => bp matches s}) => Matched(l)
    case (s :: ss, t @ SeqPat((bp @ BlockPat()) :: _)) if (bp matches s) =>
      refine(ss, t) match {
        case Matched(l) => Matched(s :: l)
        case x => x
      }
    case (l, SeqPat((bp @ BlockPat()) :: ts)) if (l.size == 0 || !(bp matches l.head)) =>
      refine(l, SeqPat(ts))
    // sequence pattern
    case (s :: ss, SeqPat((np: NonDetPat) :: ts)) =>
      (refine(List(s), np), refine(ss, SeqPat(ts))) match {
        case (Matched(a), Matched(b)) => Matched(a ::: b)
        case _ => Unmatched(np)
      }
    case (s :: ss, SeqPat((ip: IfPat) :: ts)) =>
      (refine(List(s), ip), refine(ss, SeqPat(ts))) match {
        case (Matched(a), Matched(b)) => Matched(a ::: b)
        case _ => Unmatched(ip)
      }
    case (l, SeqPat(InsertPat(code) :: ts)) =>
      refine(l, SeqPat(ts)) match {
        case Matched(a) => Matched(RefinementBlock(code, Nil) :: a)
        case x => x
      }
    case (s :: ss, SeqPat((wp: WhilePat) :: ts)) =>
      (refine(List(s), wp), refine(ss, SeqPat(ts))) match {
        case (Matched(a), Matched(b)) => Matched(a ::: b)
        case _ => Unmatched(wp)
      }
    case (_, t) => Unmatched(t)
  }

  /**
   * Transforms an expression using f. f must produce expressions of the appropriate type (e.g. not replace int literal with a bool literal)
   * Ensures that mutable fields of expressions are carried over. f must make sure that mutable fields of its value are filled in.
   */
  def transform(expr: Expression, f: Expression => Option[Expression]):Expression = {
    val func = (e:Expression) => transform(e, f);
    val x = f(expr);
    // apply recursively
    val result = if (x isDefined) x.get else expr match {
      case _:Literal => expr
      case _:ThisExpr => expr
      case _:Result => expr
      case _:VariableExpr => expr
      case ma@BackPointerMemberAccess(e, typeId, fieldId) => BackPointerMemberAccess(func(e), typeId, fieldId)
      case ma@MemberAccess(e, id) =>
        val g = MemberAccess(func(e), id)
        g.f = ma.f
        g.predicate = ma.predicate
        g.isPredicate = ma.isPredicate
        g
      case ForkEpsilon(token) => ForkEpsilon(func(token))
      case MonitorEpsilon(Some(monitor)) => MonitorEpsilon(Some(func(monitor)))
      case ChannelEpsilon(Some(channel)) => ChannelEpsilon(Some(func(channel)))
      case PredicateEpsilon(Some(predicate)) => PredicateEpsilon(Some(func(predicate)))
      case ChannelEpsilon(None) | MonitorEpsilon(None) | PredicateEpsilon(None) => expr
      case Full | Star | Epsilon | MethodEpsilon => expr
      case Frac(perm) => Frac(func(perm))
      case Epsilons(perm) => Epsilons(func(perm))
      case PermTimes(lhs, rhs) => PermTimes(func(lhs).asInstanceOf[Permission], func(rhs).asInstanceOf[Permission])
      case IntPermTimes(lhs, rhs) => IntPermTimes(func(lhs), func(rhs).asInstanceOf[Permission])
      case PermPlus(lhs, rhs) => PermPlus(func(lhs).asInstanceOf[Permission], func(rhs).asInstanceOf[Permission])
      case PermMinus(lhs, rhs) => PermMinus(func(lhs).asInstanceOf[Permission], func(rhs).asInstanceOf[Permission])
      case Access(e, perm) => Access(func(e).asInstanceOf[MemberAccess], func(perm).asInstanceOf[Permission])
      case BackPointerAccess(e, perm) =>
        BackPointerAccess(func(e).asInstanceOf[BackPointerMemberAccess], func(perm).asInstanceOf[Permission])
      case AccessAll(obj, perm) => AccessAll(func(obj), func(perm).asInstanceOf[Permission]);
      case AccessSeq(s, None, perm) => AccessSeq(func(s), None, func(perm).asInstanceOf[Permission])
      case AccessSeq(s, Some(f), perm) => AccessSeq(func(s), Some(func(f).asInstanceOf[MemberAccess]), func(perm).asInstanceOf[Permission])
      case Credit(e, None) => Credit(func(e), None)
      case Credit(e, Some(n)) => Credit(func(e), Some(func(n)))
      case Holds(e) => Holds(func(e))
      case RdHolds(e) => RdHolds(func(e))
      case _: Assigned => expr
      case Old(e) => Old(func(e))
      case IfThenElse(con, thn, els) => IfThenElse(func(con), func(thn), func(els))
      case Not(e) => Not(func(e))
      case funapp@FunctionApplication(obj, id, args) =>
        val appl = FunctionApplication(func(obj), id, args map { arg => func(arg)});
        appl.f = funapp.f;
        appl
      case Unfolding(pred, e) =>
        Unfolding(func(pred).asInstanceOf[Access], func(e))
      case Iff(e0,e1) => Iff(func(e0), func(e1))
      case Implies(e0,e1) => Implies(func(e0), func(e1))
      case And(e0,e1) => And(func(e0), func(e1))
      case Or(e0,e1) => Or(func(e0), func(e1))
      case Eq(e0,e1) => Eq(func(e0), func(e1))
      case Neq(e0,e1) => Neq(func(e0), func(e1))
      case Less(e0,e1) => Less(func(e0), func(e1))
      case AtMost(e0,e1) => AtMost(func(e0), func(e1))
      case AtLeast(e0,e1) => AtLeast(func(e0), func(e1))
      case Greater(e0,e1) => Greater(func(e0), func(e1))
      case LockBelow(e0,e1) => LockBelow(func(e0), func(e1))
      case Plus(e0,e1) => Plus(func(e0), func(e1))
      case Minus(e0,e1) => Minus(func(e0), func(e1))
      case Times(e0,e1) => Times(func(e0), func(e1))
      case Div(e0,e1) => Div(func(e0), func(e1))
      case Mod(e0,e1) => Mod(func(e0), func(e1))
      case ExplicitSeq(es) => ExplicitSeq(es map { e => func(e) })
      case Range(min, max)=> Range(func(min), func(max))
      case Append(e0, e1) => Append(func(e0), func(e1))
      case At(e0, e1) => At(func(e0), func(e1))
      case Drop(e0, e1) => Drop(func(e0), func(e1))
      case Take(e0, e1) => Take(func(e0), func(e1))
      case Length(e) => Length(func(e))
      case Contains(e0, e1) => Contains(func(e0), func(e1))
      case qe @ SeqQuantification(q, is, seq, e) =>
        val result = SeqQuantification(q, is, func(seq), func(e));
        result.variables = qe.variables;
        result;
      case qe @ SetQuantification(q, is, set, e) =>
        val result = SetQuantification(q, is, func(set), func(e));
        result.variables = qe.variables;
        result;
      case qe @ TypeQuantification(q, is, t, e, (min, max)) =>
        val result = TypeQuantification(q, is, t, func(e), (func(min),func(max)));
        result.variables = qe.variables;
        result;
      case qe @ TypeQuantification(q, is, t, e, null) =>
        val result = new TypeQuantification(q, is, t, func(e));
        result.variables = qe.variables;
        result;
      case Eval(h, e) =>
        Eval(h match {
          case AcquireState(obj) => AcquireState(func(obj))
          case ReleaseState(obj) => ReleaseState(func(obj))
          case cs @ CallState(token, obj, i, args) =>
            val result = CallState(func(token), func(obj), i, args map { a => func(a)});
            result.m = cs.m;
            result;
        }, func(e))
      case _ => throw new NotSupportedException("encountered non-supported feature")
    };

    // preserve type
    if (result.typ == null) result.typ = expr.typ;
    // preserve position
    if (result.pos == NoPosition) result.pos = expr.pos
    result
  }

  // Applies recursively the function f first to the expression and thn to its subexpressions (that is members of type RValue)
  def visit(expr: RValue, f: RValue => Unit) = visitOpt(expr, r => {f(r); true})
  // Applies recursively the function f first to the expression and thn to its subexpressions (that is members of type RValue)
  def visit2(expr: RValue, f: RValue => Unit, f2: RValue => Unit) = visitOpt2(expr, r => {f(r); true}, f2)
  // Applies recursively the function f first to the expression and, if f returns true, thn to its subexpressions
  def visitOpt(expr: RValue, f: RValue => Boolean) = visitOpt2(expr, f, _ => Unit)
  // Applies recursively the function f first to the expression and, if f returns true, thn to its subexpressions.  Finally, f2 is applied to the expression.
  def visitOpt2(expr: RValue, f: RValue => Boolean, f2: RValue => Unit) {
    if (f(expr)) {
      expr match {
         case _:Literal => ;
         case _:ThisExpr => ;
         case _:Result => ;
         case _:VariableExpr => ;
         case BackPointerMemberAccess(e, _, _) =>
           visitOpt2(e, f, f2);
         case MemberAccess(e, _) =>
           visitOpt2(e, f, f2);
         case Frac(p) => visitOpt2(p, f, f2);
         case Epsilons(p) => visitOpt2(p, f, f2);
         case Full | Epsilon | Star | MethodEpsilon =>;
         case ChannelEpsilon(None) | PredicateEpsilon(None) | MonitorEpsilon(None) =>;
         case ChannelEpsilon(Some(e)) => visitOpt2(e, f, f2);
         case PredicateEpsilon(Some(e)) => visitOpt2(e, f, f2);
         case MonitorEpsilon(Some(e)) => visitOpt2(e, f, f2);
         case ForkEpsilon(tk) => visitOpt2(tk, f, f2);
         case IntPermTimes(n, p) =>
           visitOpt2(n, f, f2); visitOpt2(p, f, f2);
         case PermTimes(e0, e1) =>
           visitOpt2(e0, f, f2); visitOpt2(e1, f, f2);
         case PermPlus(e0, e1) =>
           visitOpt2(e0, f, f2); visitOpt2(e1, f, f2);
         case PermMinus(e0, e1) =>
           visitOpt2(e0, f, f2); visitOpt2(e1, f, f2);
         case Access(e, perm) =>
           visitOpt2(e, f, f2); visitOpt2(perm, f, f2);
         case AccessAll(obj, perm) =>
           visitOpt2(obj, f, f2); visitOpt2(perm, f, f2);
         case AccessSeq(s, _, perm) =>
           visitOpt2(s, f, f2); visitOpt2(perm, f, f2);

         case Credit(e, n) =>
           visitOpt2(e, f, f2); n match { case Some(n) => visitOpt2(n, f, f2); case _ => }
         case Holds(e) => visitOpt2(e, f, f2);
         case RdHolds(e) => visitOpt2(e, f, f2);

         case e: BinaryExpr =>
           visitOpt2(e.E0, f, f2); visitOpt2(e.E1, f, f2);
         case Range(min, max) =>
           visitOpt2(min, f, f2); visitOpt2(max, f, f2);
         case e: Assigned => e
         case Old(e) => visitOpt2(e, f, f2);
         case IfThenElse(con, thn, els) => visitOpt2(con, f, f2); visitOpt2(thn, f, f2); visitOpt2(els, f, f2);
         case Not(e) => visitOpt2(e, f, f2);
         case funapp@FunctionApplication(obj, id, args) =>
           visitOpt2(obj, f, f2); args foreach { arg => visitOpt2(arg, f, f2) };
         case Unfolding(pred, e) =>
           visitOpt2(pred, f, f2); visitOpt2(e, f, f2);

         case SeqQuantification(_, _, seq, e) => visitOpt2(seq, f, f2); visitOpt2(e, f, f2);
         case SetQuantification(_, _, seq, e) => visitOpt2(seq, f, f2); visitOpt2(e, f, f2);
         case TypeQuantification(_, _, _, e, (min,max)) => visitOpt2(e, f, f2); visitOpt2(min, f, f2); visitOpt2(max, f, f2);
         case TypeQuantification(_, _, _, e, _) => visitOpt2(e, f, f2);
         case ExplicitSeq(es) =>
           es foreach { e => visitOpt2(e, f, f2) }
         case Length(e) =>
           visitOpt(e, f)
         case Eval(h, e) =>
           h match {
             case AcquireState(obj) => visitOpt2(obj, f, f2);
             case ReleaseState(obj) => visitOpt2(obj, f, f2);
             case CallState(token, obj, id, args) =>
               visitOpt2(token, f, f2); visitOpt2(obj, f, f2); args foreach {a : Expression => visitOpt2(a, f, f2)};
           }
           visitOpt2(e, f, f2);
         case NewRhs(_, init, lowerBounds, upperBounds) =>
           lowerBounds foreach { e => visitOpt2(e, f, f2)};
           upperBounds foreach { e => visitOpt2(e, f, f2)};

         case ExplicitSet(es) =>
           es foreach { e => visitOpt2(e, f, f2) }
     }
   }
   f2(expr)
 }
}
