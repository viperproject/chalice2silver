
package semper {

import chalice.ASTNode
import chalice2sil.translation.{FunctionTranslator, PredicateTranslator, MethodTranslator, FieldTranslator}
import semper.sil.ast.source.SourceLocation
import scala.util.parsing.input.Positional
import collection.GenTraversableLike
import semper.sil.ast.methods.MethodFactory
import semper.sil.ast.programs.symbols.{FunctionFactory, PredicateFactory, Field}
import semper.sil.ast.expressions.terms.{PredicateLocation, Term, FieldLocation, Location}

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

    def getReceiverFromLocation(location : Location) : Term = location match {
      case FieldLocation(receiver,_) => receiver
      case PredicateLocation(receiver,_) => receiver
      case _ => throw new Error("Don't know how to access receiver of location " + location + ".")
    }
  }
}