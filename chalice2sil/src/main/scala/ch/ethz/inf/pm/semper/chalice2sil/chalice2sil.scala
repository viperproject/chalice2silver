
package ch.ethz.inf.pm.semper {

import chalice.ASTNode
import chalice2sil.translation.{FunctionTranslator, PredicateTranslator, MethodTranslator, FieldTranslator}
import silAST.source.SourceLocation
import scala.util.parsing.input.Positional
import collection.GenTraversableLike
import silAST.methods.MethodFactory
import silAST.programs.symbols.{FunctionFactory, PredicateFactory, Field}

package object chalice2sil {
    //at some point, use the node's Positional trait to provide a source location.
    implicit def astNodeToSourceLocation(p : ASTNode) : SourceLocation = new SourceLocation {
      val pos = p.pos
      override def toString = "%d.%d".format(pos.line,pos.column)
    }
    implicit def astNodeToOptionalSourceLocation(p : ASTNode) : Option[SourceLocation] =
      Some(astNodeToSourceLocation(p))
  
    implicit def unwrapField(ft : FieldTranslator) : Field = ft.field
    implicit def unwrapMethod(mt : MethodTranslator) : MethodFactory = mt.methodFactory
    implicit def unwrapPredicate(pt : PredicateTranslator) : PredicateFactory = pt.predicateFactory
    implicit def unwrapFunction(ft : FunctionTranslator) : FunctionFactory = ft.functionFactory
  }
}