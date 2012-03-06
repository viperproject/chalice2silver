
package ch.ethz.inf.pm.semper {

import chalice.ASTNode
import chalice2sil.translation.{MethodTranslator, FieldTranslator}
import silAST.source.SourceLocation
import util.parsing.input.{Positional}
import collection.GenTraversableLike
import silAST.programs.symbols.Field
import silAST.methods.MethodFactory

package object chalice2sil {
    //at some point, use the node's Positional trait to provide a source location.
    implicit def astNodeToSourceLocation(p : ASTNode) : SourceLocation = new SourceLocation {
      val pos = p.pos
      override def toString = pos.toString()
    }
    implicit def astNodeToOptionalSourceLocation(p : ASTNode) : Option[SourceLocation] =
      Some(astNodeToSourceLocation(p))
  
    implicit def unwrapField(ft : FieldTranslator) : Field = ft.field
    implicit def unwrapMethod(mt : MethodTranslator) : MethodFactory = mt.methodFactory
  }
}