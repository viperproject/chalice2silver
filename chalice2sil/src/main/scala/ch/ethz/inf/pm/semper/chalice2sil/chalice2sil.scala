/**
 * Author: Christian Klauser
 * Date: 23.01.12
 */
package ch.ethz.inf.pm.semper {

import chalice.ASTNode
import silAST.source.SourceLocation
import util.parsing.input.{Positional}

package object chalice2sil {
    //at some point, use the node's Positional trait to provide a source location.
    implicit def astNodeToSourceLocation(p : ASTNode) : SourceLocation = new SourceLocation {
      val pos = p.pos
      override def toString = pos.toString()
    }
  }
}