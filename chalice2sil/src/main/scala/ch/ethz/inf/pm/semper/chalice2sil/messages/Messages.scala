package ch.ethz.inf.pm.semper.chalice2sil.messages

import ch.ethz.inf.pm.semper.chalice2sil._
import chalice.{Channel, ASTNode}
import silAST.source.SourceLocation
import Severity._

object ChannelsNotImplemented extends MessageId(
  Error,
  "ch.ethz.inf.pm.semper.chalice2sil.channelsNotImplemented",
  "Could not translate definition of channel %s.") {
  def apply(channel : Channel) = new Message(this,channel) {
    def data: Iterable[Any] = Array(channel.channelId)
  }
}

object UnknownAstNode extends MessageId(
  Error,
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