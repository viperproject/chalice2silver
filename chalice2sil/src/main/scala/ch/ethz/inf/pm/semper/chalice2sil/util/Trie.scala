package ch.ethz.inf.pm.semper.chalice2sil.util

import collection.mutable.ArrayStack


class Trie[K <: Seq[E], E, V] private(val emptyValue : Option[V], val firstLevel : Map[E, TrieNode[E, V]])
  extends Traversable[TrieNode[E, V]] {
  def shortestMatch(window : Iterable[E]) : Option[V] = emptyValue match {
    case s@Some(_) => s
    case None =>
      val i = window.iterator
      if(!i.hasNext)
        None
      else
        firstLevel.get(i.next()).flatMap(shortestMatch(i,_))
  }
  
  private[this] def shortestMatch(i : Iterator[E], trie : TrieNode[E,V]) : Option[V] = {
    trie.value match {
      case s@Some(_) => s
      case _ if i.hasNext =>
        val e = i.next()
        trie.children.find(_.element == e).flatMap(shortestMatch(i,_))
      case _ => None
    }
  }
  
  override def foreach[U](action : TrieNode[E, V] => U) {
    val q = collection.mutable.Queue(firstLevel.values.toSeq:_*)
    while(!q.isEmpty){
      val t = q.dequeue()
      action(t)
      q.enqueue(t.children:_*)
    }
  }

  def allowsShortestMatch : Option[TrieNode[E,V]] = find(t => (t.value.isDefined) && (!t.children.isEmpty))
}

object Trie {
  def create[K <: Seq[E],E,V](map : Iterable[(K, V)]) : Trie[K, E, V] = {
    val lengthMap = map.groupBy(_._1.length)
    val maxLength = lengthMap.keysIterator.max
    type Key = List[E]
    type Branches = List[TrieNode[E, V]]
    type State = Map[Key,Branches]
    def combine(m : State, t : (Key,Branches)) : State = {
      val (key,branches) = t
      val bs = m.getOrElse(key.tail, List())
      var attached = false
      val bs1 = bs.collect {
        case o@TrieNode(e1,v,children) if e1 == key.head =>
          assert(!attached)
          attached = true
          TrieNode(e1,v,branches ++ children)
        case trie => trie
      }
      val upd = if (attached) {
        bs1
      } else {
        TrieNode(key.head,None,branches) :: bs
      }
      m.updated(key.tail, upd)
    }
    def initFromLength(length : Int) : State = lengthMap.get(length).map(_.map{
      case (k,v) =>
        val key = k.reverseIterator.toList
        (key.tail,List(TrieNode(key.head,Some(v),List())))
    }.toMap).getOrElse(Map())
    var currentItems : State = initFromLength(maxLength)
    for (len <- (maxLength-1).to(1,-1)){
      currentItems = currentItems.foldLeft(initFromLength(len))(combine(_,_))
    }
    new Trie[K, E, V](
      lengthMap.get(0).flatMap(_.map(_._2).headOption),
      currentItems.get(List()).map(_.map(t => (t.element,t)).toMap).getOrElse(Map()))
  }
}
