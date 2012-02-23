package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.methods.implementations.BasicBlockFactory

/**
 * Author: Christian Klauser
 */

trait AdjustableCache[V] {
  def addExternal(value : V)
}