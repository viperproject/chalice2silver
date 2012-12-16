package semper.chalice2sil.messages

import collection.mutable.{SynchronizedMap, HashMap}
import Severity._

abstract class MessageId(val defaultSeverity : Severity, val id : String, val messageFormat : String){
  MessageId.registry.synchronized({
    require(!MessageId.registry.contains(id),"A message with id %s has already been defined.".format(id))
    MessageId.registry += id -> this
  })
}

object MessageId {
  private val registry = new HashMap[String, MessageId] with SynchronizedMap[String, MessageId];
}