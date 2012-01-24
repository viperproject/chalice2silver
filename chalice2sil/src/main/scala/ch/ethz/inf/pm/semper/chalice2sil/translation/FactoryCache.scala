package ch.ethz.inf.pm.semper.chalice2sil.translation

import collection.mutable.HashMap

class FactoryCache[K,V] protected (ctor : K => V) extends (K => V) {
  protected val cache = new HashMap[K, V]
  def apply(key : K) = cache.get(key) match {
    case Some(value) => value
    case None        =>
      val value = ctor(key)
      cache += key -> value
      value
  }
}

object FactoryCache {
  def apply[K,V](ctor: K => V) = new FactoryCache[K, V](ctor)
}