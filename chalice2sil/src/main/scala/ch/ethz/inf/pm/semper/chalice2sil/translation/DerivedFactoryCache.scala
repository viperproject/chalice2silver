package ch.ethz.inf.pm.semper.chalice2sil.translation

/**
 * Author: Christian Klauser
 */

trait DerivedFactoryCache[P,K,V] extends FactoryCache[K, V] {
  def deriveKey(prototype : P) : K
  def getFromPrototype(prototype : P) : V = getOrElseUpdate(deriveKey(prototype))
}