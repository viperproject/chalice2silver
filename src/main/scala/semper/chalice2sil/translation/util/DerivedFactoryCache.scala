package semper.chalice2sil.translation.util

import collection.immutable.HashMap
import java.util.concurrent.atomic.AtomicInteger

/**
  * The derived factory cache acts much like an ordinary FactoryHashCache, with the difference that
  * values (`V`) cannot just be derived from keys (`K`). Instead they must be constructed from "prototypes" (`P`).
  *
  * Keys can also be derived from prototypes, making the derived factory cache behave like a map from `P` to `V`.
  *
  * This is useful if the prototypes are not good keys for a map, e.g., because they don't have a
  * sensible definition of equals, are mutable, or because they are too specific (when you want multiple `P` to map to the same `V`)
  *
  * If you also want to implement the `AdjustableCache` trait, override the method  `deriveKeyFromValue`.
  *
  * @author Christian Klauser
  * @tparam P The "prototype" from which a key/value pair can be derived.
  * @tparam K The key used for identifying entries.
  * @tparam V The type of elements in the cache.
  */
abstract class DerivedFactoryCache[P, K, V] extends FactoryCache[P, V] {
  private var internalCache = HashMap[K, V]()

  def getOrElseUpdate(key : P) = {
    val hashKey = deriveKey(key)
    internalCache.get(hashKey)
      .orElse(outerScope(hashKey))
      .getOrElse({
        val v = construct(key)
        internalCache += hashKey → v
        v
    })
  }

  /**
    * Adds the specified value to the cache. The corresponding key is obtained by calling `deriveKeyFromValue`.
    * @param value The value to be added to the cache.
    * @throws IllegalArgumentException An entry with this key already exists.
    */
  def addExternal(value : V) {
    val key = deriveKeyFromValue(value)
    if(internalCache contains key)
      throw new
          java.lang.IllegalArgumentException("The derived factory cache of type %s already has an entry for '%s' (trying to manually add an entry).".format(getClass.getName, key))
    internalCache += key → value
  }

  /**
    * Determines the appropriate key for a value.
    *
    * By default, this method throws an `UnsupportedOperationException`.
    * If you want the functionality of the `AdjustableCache` trait, override this method and provide an implementation.
    * @param value The value for which the key should be derived.
    * @return The (unique) key for this value.
    */
  protected def deriveKeyFromValue(value : V) : K = {
    throw new
        java.lang.UnsupportedOperationException("The type %s has not defined the optional method `deriveKeyFromValue`.".format(getClass.getName))
  }

  /**
    * A mapping from keys to values for direct lookup via keys (`K`)
    * @return mapping from keys to values.
    */
  def lookup(key : K) : V = internalCache(key)

  private val nextId = new AtomicInteger(0)

  /**
    * As an optional service, [[semper.chalice2sil.translation.util.DerivedFactoryCache]] offers unique id's.
    * @return one of 2**16-1 unique id's
    */
  def getNextId : Int = nextId.getAndIncrement

  /**
    * Derives the key from a supplied prototype.
    *
    * Note that, while prototypes may be mutable, the key derived from it should be the same while the prototype is in
    * the same state, i.e., you can't just return an incrementing integer. Otherwise the following code would not work:
    * {{{
    *   val cache = new DerivedFactoryCache[P,K,V] {...}
    *   val prototype = ...
    *   assert(cache(prototype) == cache(prototype)) //deriveKey must return the same key for both lookups
    * }}}

    }* @param p The prototype to derive a key from.
    * @return the key for the prototype
    */
  protected def deriveKey(p : P) : K

  /**
    * In case of a cache-miss, this method is called to create the missing entry. The key with which the value will
    * be added to the cache has been determined before via the `deriveKey` method.
    * @param p  The prototype to create the value from.
    * @return The value to be added to the cache.
    */
  protected def construct(p : P) : V

  /**
    * Can be overridden to forward entries from a "parent" cache. New entries will always be created locally.
    * [[semper.chalice2sil.translation.util.DerivedFactoryCache.addExternal]] can create shadowing
    * entries.
    * @param key The key to look up in the outer scope.
    * @return Some(value) if the outer scope can supply a value for the requested key, None otherwise.
    */
  protected def outerScope(key : K) : Option[V] = None
}