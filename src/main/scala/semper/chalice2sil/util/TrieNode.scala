package semper.chalice2sil.util

import collection._;
import immutable.LinearSeq
import mutable.{HashMap, ArrayBuilder}

/**
  * @author Christian Klauser
  */
case class TrieNode[E,V](element : E, value : Option[V], children : List[TrieNode[E, V]]) {

}




