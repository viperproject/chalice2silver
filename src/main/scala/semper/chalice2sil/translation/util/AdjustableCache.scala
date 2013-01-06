package semper.chalice2sil.translation.util


/**
  * Author: Christian Klauser
  */

trait AdjustableCache[V] {
  def addExternal(value : V)
}