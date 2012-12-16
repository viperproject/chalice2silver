package semper.chalice2sil.translation.util

import semper.sil.ast.methods.implementations.BasicBlockFactory

/**
  * Author: Christian Klauser
  */

trait AdjustableCache[V] {
  def addExternal(value : V)
}