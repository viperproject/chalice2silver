package ch.ethz.inf.pm.semper.chalice2sil.translation

import collection.mutable.HashMap

trait FactoryCache[K,V] {
  def getOrElseUpdate(key : K) : V
  @inline
  def apply(key : K) = getOrElseUpdate(key)
}

abstract class FactoryHashCache[K,V] extends FactoryCache[K, V] {
  protected val cache = new HashMap[K, V]
  protected def construct(key : K) : V
  def getOrElseUpdate(key : K) = cache.getOrElseUpdate(key, construct(key))
}

abstract class AdjustableFactoryHashCache[K,V] extends FactoryHashCache[K, V] with AdjustableCache[V] {
  def addExternal(value : V) {
    cache.update(getKeyFor(value),value)
  }
  def getKeyFor(value : V) : K
}