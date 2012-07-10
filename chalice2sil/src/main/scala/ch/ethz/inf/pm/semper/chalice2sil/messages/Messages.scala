package ch.ethz.inf.pm.semper.chalice2sil.messages

import ch.ethz.inf.pm.semper.chalice2sil._
import chalice.{Channel, ASTNode}
import silAST.source.SourceLocation
import Severity._
import silAST.types.DataType
import silAST.expressions.terms.{LogicalVariableTerm, PTerm, Term}
import translation.LocationTranslator

object ChannelsNotImplemented extends MessageId(
  Error,
  "ch.ethz.inf.pm.semper.chalice2sil.channelsNotImplemented",
  "Could not translate definition of channel %s.") {
  def apply(channel : Channel) = new Message(this,channel) {
    def data: Iterable[Any] = Array(channel.channelId)
  }
}

object UnknownAstNode extends MessageId(
  Fault,
  "ch.ethz.inf.pm.semper.chalice2sil.unknownAstNode",
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
  "ch.ethz.inf.pm.semper.chalice2sil.freeVariableInOld",
  "Chalice2SIL cannot handle free variables in old expressions and terms. Offending variable: %s") {
  def apply(variableTerm : LogicalVariableTerm) = new Message(this,variableTerm.sourceLocation) {
    def data = Array(variableTerm)
  }
}

object ContractNotUnderstood extends MessageId(
  Fault,
"ch.ethz.inf.pm.semper.chalice2sil.contractNotUnderstood",
"Chalice2SIL does not understand the contract expression %s.") {
  def apply(node : silAST.ASTNode) = new Message(this,node.sourceLocation) {
    def data = Array(node)
  }
}

object TypeNotUnderstood extends MessageId(
  Fault,
"ch.ethz.inf.pm.semper.chalice2sil.typeNotUnderstood",
"Chalice2SIL does not understand the SIL type %s, derived from node %s.") {
  def apply(dataType : DataType, node : ASTNode) = new Message(this,node) {
    def data : Iterable[Any] = Array(dataType, node)
  }
}

object PermissionNotUnderstood extends MessageId(
  Fault,
  "ch.ethz.inf.pm.semper.chalice2sil.permissionNotUnderstood",
  "Chalice2SIL does not understand the SIL permission amount %s.") {
  def apply(location : SourceLocation, permissionAmount : silAST.ASTNode) = new Message(this,location) {
    def data : Iterable[Any] = Array(permissionAmount)
  }
}

object RdInQuantifier extends MessageId(
  Error,
  "ch.ethz.inf.pm.semper.chalice2sil.rdInQuantifier",
  "Cannot translate fractional read permission rd(%s.%s, %s) in quantified expressions.") {
  def apply(reference : PTerm, location : LocationTranslator, permissionAmount : PTerm) = new Message(this,reference.sourceLocation) {
    def data = Array(reference,location,permissionAmount)
  }
}

object NoContextForReadPermission extends MessageId(
Fault,"ch.ethz.inf.pm.semper.chalice2sil.noContextForReadPermission","Unknown interpretation for read permission at %s.") {
 def apply(location : SourceLocation) = new Message(this,location) {
   def data = Array(location)
 }
}

object OperatorNotFound extends MessageId(Error,
"ch.ethz.inf.pm.semper.chalice2sil.operatorNotFound",
"Cannot find operator \"%s\" implementing (%s,%s) => %s."){
  def apply(binaryExpr : chalice.BinaryExpr, lhs : DataType, rhs : DataType, result : DataType) = new Message(this,binaryExpr) {
    def data : Iterable[Any] = Array(binaryExpr.OpName,lhs,rhs,result)
  }
}

object TermInExpressionPosition extends MessageId(
  Fault,
  "ch.ethz.inf.pm.semper.chalice2sil.termInExpressionPosition",
  "The chalice node %s has type %s but is used in a position where SIL expects an expression.") {
  def apply(expr : chalice.Expression, dataType : DataType) = new Message(this,expr) {
    def data : Iterable[Any] = Array(expr,dataType)
  }
}

object LockingRelatedInPredicate extends MessageId(
  Fault,
  "ch.ethz.inf.pm.semper.chalice2sil.lockingRelatedInPredicate",
  "Due to an internal limitation, Chalice2SIL cannot translate certain locking-related language constructs in predicates or invariants.") {
  def apply(location : SourceLocation) = new Message(this,location) {
    def data = Nil
  }
}

object RdLockNotSupported extends MessageId(
  Fault,
  "ch.ethz.inf.pm.semper.chalice2sil.rdLockNotSupported",
  "Rd-locks (acquire, release, lock) are not supported by chalice2sil.") {
  def apply(location : SourceLocation) = new Message(this,location) {
    def data = Nil
  }
}