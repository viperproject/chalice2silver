package semper.chalice2sil.messages

import semper.chalice2sil._
import chalice.{Channel, ASTNode}
import semper.sil.ast._
import Severity._

object ChannelsNotImplemented extends MessageId(
  Fault,
  "semper.chalice2sil.channelsNotImplemented",
  "Could not translate definition of channel %s.") {
  def apply(channel : Channel) = new Message(this, null) {
    override val position = SourcePosition(null, channel.pos.line, channel.pos.column)
    def data: Iterable[Any] = Array(channel.channelId)
  }
}

object GeneralEvalNotImplemented extends MessageId(
  Fault,
  "semper.chalice2sil.generalEvalNotImplemented",
  "Chalice2SIL currently does not support eval.  Offending expression: %s.") {
  def apply(eval : chalice.Eval) = new Message(this, eval) {
    def data : Iterable[Any] = Array(eval)
  }
}

object SequenceQuantificationNotImplemented extends MessageId(
  Fault,
  "semper.chalice2sil.sequenceQuantificationNotSupported",
  "Chalice2SIL currently does not understand quantification over sequences. Offending expression: %s.") {
  def apply(eval : chalice.Quantification) = new Message(this, eval) {
    def data : Iterable[Any] = Array(eval)
  }
}

object UnknownAstNode extends MessageId(
  Fault,
  "semper.chalice2sil.unknownAstNode",
  "Could not translate AST node of type %s. Value: (%s)") {
  def apply(node : ASTNode) = new Message(this, node) {
    def data: Iterable[Any] = Array(node.getClass,etc(node), node)
  }
  
  private[messages] def etc(x:Any,limit:Int = 27) = {
    val s  = x.toString
    if (s.length > limit)
      s.take(limit-1)  + "…"
    else
      s
  }
}

object FreeVariableInOld extends MessageId(
  Error,
  "semper.chalice2sil.freeVariableInOld",
  "Chalice2SIL cannot handle free variables in old expressions and terms. Offending variable: %s") {
  def apply() = new Message(this, null) {
    def data = Array[Any]()
  }
}

object PredicateScalingNotSupported extends MessageId(Fault,"semper.chalice2sil.predicateScalingNotSupported",
  "Chalice2SIL does not fully support predicate scaling. acc(%s.%s, %s)") {
  def apply(location1 : semper.sil.ast.Exp, pred : semper.sil.ast.Predicate, amount : semper.sil.ast.Exp) =
    new Message(this, null) {
    def data = Array(location1, pred.name, amount,pred)
  }
}

object ContractNotUnderstood extends MessageId(
  Fault,
"semper.chalice2sil.contractNotUnderstood",
"Chalice2SIL does not understand the contract expression %s.") {
  def apply(node : semper.sil.ast.Node) = new Message(this, null) {
    def data = Array(node)
  }
}

object TypeNotUnderstood extends MessageId(
  Fault,
"semper.chalice2sil.typeNotUnderstood",
"Chalice2SIL does not understand the SIL type %s, derived from node %s.") {
  def apply(dataType: Type, node : ASTNode) = new Message(this,node) {
    def data : Iterable[Any] = Array(dataType, node)
  }
}

object PermissionNotUnderstood extends MessageId(
  Fault,
  "semper.chalice2sil.permissionNotUnderstood",
  "Chalice2SIL does not understand the SIL permission amount %s.") {
  def apply(location: Position, permissionAmount : semper.sil.ast.Node) = new Message(this,null) {
    def data : Iterable[Any] = Array(permissionAmount)
  }
}

object PermissionTooComplicatedForPredicateOrFunction extends MessageId(
  Error,
  "semper.chalice2sil.permissionTooComplicatedForPredicateOrFunction",
  "Chalice2SIL can only handle full or abstract read permission amounts in predicates and functions.") {
  def apply(permissionAmount : chalice.Permission) = new Message(this, permissionAmount) {
    def data : Iterable[Any] = Array(permissionAmount)
  }
}

object RdInQuantifier extends MessageId(
  Error,
  "semper.chalice2sil.rdInQuantifier",
  "Cannot translate fractional read permission rd(%s.%s, %s) in quantified expressions.") {
  def apply(reference : semper.sil.ast.Exp) = new Message(this, null) {
    def data = Array(reference)
  }
}

object NoContextForReadPermission extends MessageId(
Fault,"semper.chalice2sil.noContextForReadPermission","Unknown interpretation for read permission at %s.") {
 def apply(location1: Position) = new Message(this, null) {
   def data = Array(location1)
 }
}

object OperatorNotFound extends MessageId(Error,
"semper.chalice2sil.operatorNotFound",
"Cannot find operator \"%s\" implementing (%s,%s) => %s."){
  def apply(binaryExpr : chalice.BinaryExpr, lhs: Type, rhs: Type, result: Type) = new Message(this,binaryExpr) {
    def data : Iterable[Any] = Array(binaryExpr.OpName,lhs,rhs,result)
  }
}

object ExpressionInExpressionPosition extends MessageId(
  Fault,
  "semper.chalice2sil.termInExpressionPosition",
  "The chalice node %s has type %s but is used in a position where SIL expects an expression.") {
  def apply(expr : chalice.Expression, dataType: Type) = new Message(this,expr) {
    def data : Iterable[Any] = Array(expr,dataType)
  }
}

object LockingRelatedInPredicate extends MessageId(
  Fault,
  "semper.chalice2sil.lockingRelatedInPredicate",
  "Due to an internal limitation, Chalice2SIL cannot translate certain locking-related language constructs in predicates or invariants.") {
  def apply(location: Position) = new Message(this,null) {
    def data = Nil
  }
}

object RdLockNotSupported extends MessageId(
  Fault,
  "semper.chalice2sil.rdLockNotSupported",
  "Rd-locks (acquire, release, lock) are not supported by chalice2sil.") {
  def apply() = new Message(this,null) {
    def data = Nil
  }
}

object WrongNumberOfTypeParameters extends MessageId(
  Fault,
  "semper.chalice2sil.WrongNumberOfTypeParameters",
  "Sequences and Sets must have exactly one type parameter.") {
  def apply() = new Message(this,null) {
    def data = Nil
  }
}

object TypeError extends MessageId(
  Fault,
  "semper.chalice2sil.TypeError",
  "Type Error") {
  def apply() = new Message(this,null) {
    def data = Nil
  }
}