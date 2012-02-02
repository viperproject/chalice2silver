package ch.ethz.inf.pm.semper.chalice2sil.messages

import ch.ethz.inf.pm.semper.chalice2sil._
import chalice.{Channel, ASTNode}
import silAST.source.SourceLocation
import Severity._
import silAST.types.DataType

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

object TypeNotUnderstood extends MessageId(
  Fault,
"ch.ethz.inf.pm.semper.chalice2sil.typeNotUnderstood",
"Chalice2SIL does not understand the SIL type %s, derived from node %s.") {
  def apply(dataType : DataType, node : ASTNode) = new Message(this,node) {
    def data : Iterable[Any] = Array(dataType, node)
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