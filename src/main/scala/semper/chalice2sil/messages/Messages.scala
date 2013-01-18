package semper.chalice2sil.messages

import semper.chalice2sil._
import chalice.{Channel, ASTNode}
import semper.sil.ast.source.SourceLocation
import Severity._
import semper.sil.ast.types.DataType
import semper.sil.ast.expressions.terms.LogicalVariableExpression
import translation.LocationTranslator

object ChannelsNotImplemented extends MessageId(
  Fault,
  "semper.chalice2sil.channelsNotImplemented",
  "Could not translate definition of channel %s.") {
  def apply(channel : Channel) = new Message(this,channel) {
    def data: Iterable[Any] = Array(channel.channelId)
  }
}

object GeneralEvalNotImplemented extends MessageId(
  Fault,
  "semper.chalice2sil.generalEvalNotImplemented",
  "Chalice2SIL currently only understands eval(token.fork receiver.m(args...), true). Other forms of the eval " +
    "expression are not supported. Offending expression: %s.") {
  def apply(eval : chalice.Eval) = new Message(this,eval) {
    def data : Iterable[Any] = Array(eval)
  }
}

object SequenceQuantificationNotImplemented extends MessageId(
  Fault,
  "semper.chalice2sil.sequenceQuantificationNotSupported",
  "Chalice2SIL currently does not understand quantification over sequences. Offending expression: %s.") {
  def apply(eval : chalice.Quantification) = new Message(this,eval) {
    def data : Iterable[Any] = Array(eval)
  }
}

object UnknownAstNode extends MessageId(
  Fault,
  "semper.chalice2sil.unknownAstNode",
  "Could not translate AST node of type %s. Value: (%s)") {
  def apply(node : ASTNode) = new Message(this,node) {
    def data: Iterable[Any] = Array(node.getClass,etc(node),node)
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
  def apply(variableExpression : LogicalVariableExpression) = new Message(this,variableExpression.sourceLocation) {
    def data = Array(variableExpression)
  }
}

object PredicateScalingNotSupported extends MessageId(Fault,"semper.chalice2sil.predicateScalingNotSupported",
  "Chalice2SIL does not fully support predicate scaling. acc(%s.%s, %s)") {
  def apply(location1 : semper.sil.ast.expressions.Expression, pred : semper.sil.ast.programs.symbols.Predicate, amount : semper.sil.ast.expressions.Expression) =
    new Message(this, location1.sourceLocation) {
    def data = Array(location1, pred.name, amount,pred)
  }
}

object ContractNotUnderstood extends MessageId(
  Fault,
"semper.chalice2sil.contractNotUnderstood",
"Chalice2SIL does not understand the contract expression %s.") {
  def apply(node : semper.sil.ast.ASTNode) = new Message(this,node.sourceLocation) {
    def data = Array(node)
  }
}

object TypeNotUnderstood extends MessageId(
  Fault,
"semper.chalice2sil.typeNotUnderstood",
"Chalice2SIL does not understand the SIL type %s, derived from node %s.") {
  def apply(dataType : DataType, node : ASTNode) = new Message(this,node) {
    def data : Iterable[Any] = Array(dataType, node)
  }
}

object PermissionNotUnderstood extends MessageId(
  Fault,
  "semper.chalice2sil.permissionNotUnderstood",
  "Chalice2SIL does not understand the SIL permission amount %s.") {
  def apply(location : SourceLocation, permissionAmount : semper.sil.ast.ASTNode) = new Message(this,location) {
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
  def apply(reference : semper.sil.ast.expressions.Expression, location1 : LocationTranslator, permissionAmount : semper.sil.ast.expressions.Expression) = new Message(this,reference.sourceLocation) {
    def data = Array(reference,location1,permissionAmount)
  }
}

object NoContextForReadPermission extends MessageId(
Fault,"semper.chalice2sil.noContextForReadPermission","Unknown interpretation for read permission at %s.") {
 def apply(location1 : SourceLocation) = new Message(this,location1) {
   def data = Array(location1)
 }
}

object OperatorNotFound extends MessageId(Error,
"semper.chalice2sil.operatorNotFound",
"Cannot find operator \"%s\" implementing (%s,%s) => %s."){
  def apply(binaryExpr : chalice.BinaryExpr, lhs : DataType, rhs : DataType, result : DataType) = new Message(this,binaryExpr) {
    def data : Iterable[Any] = Array(binaryExpr.OpName,lhs,rhs,result)
  }
}

object ExpressionInExpressionPosition extends MessageId(
  Fault,
  "semper.chalice2sil.termInExpressionPosition",
  "The chalice node %s has type %s but is used in a position where SIL expects an expression.") {
  def apply(expr : chalice.Expression, dataType : DataType) = new Message(this,expr) {
    def data : Iterable[Any] = Array(expr,dataType)
  }
}

object LockingRelatedInPredicate extends MessageId(
  Fault,
  "semper.chalice2sil.lockingRelatedInPredicate",
  "Due to an internal limitation, Chalice2SIL cannot translate certain locking-related language constructs in predicates or invariants.") {
  def apply(location : SourceLocation) = new Message(this,location) {
    def data = Nil
  }
}

object RdLockNotSupported extends MessageId(
  Fault,
  "semper.chalice2sil.rdLockNotSupported",
  "Rd-locks (acquire, release, lock) are not supported by chalice2sil.") {
  def apply(location : SourceLocation) = new Message(this,location) {
    def data = Nil
  }
}