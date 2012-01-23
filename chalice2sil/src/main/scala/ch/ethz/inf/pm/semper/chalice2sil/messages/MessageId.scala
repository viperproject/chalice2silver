package ch.ethz.inf.pm.semper.chalice2sil.messages

import collection.mutable.{SynchronizedMap, HashMap}

/**
 * Author: Christian Klauser
 * Date: 23.01.12
 */

abstract class MessageId(val defaultSeverity : Severity, val id : String, val messageFormat : String){
  MessageId.registry.synchronized(
    if(MessageId.registry.contains(id))
      throw new Exception("Message with id "  + id + " already defined: " + MessageId.registry(id))
    else {
      MessageId.registry += id -> this
    }
  )
}

object MessageId {
  private val registry = new HashMap[String, MessageId] with SynchronizedMap[String, MessageId];
}