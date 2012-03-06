package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.source.noLocation
import silAST.expressions.ExpressionFactory
import silAST.expressions.terms.{PTerm, Term}
import silAST.expressions.util.{PTermSequence, TermSequence}

/**
  * @author Christian Klauser
  */
class FieldTranslator(
                       /**
                         * The underlying Chalice field
                         */
                       val sourceField : chalice.Field,

                       /**
                         * An id that is unique within the program and is used to identify this field
                         * in expressions.
                         */
                       val id : Int, programEnvironment : ProgramEnvironment)
  extends DerivedProgramEnvironment(programEnvironment)
  with TypeTranslator {
  val field = programFactory.defineField(noLocation,fullFieldName(sourceField),translateTypeExpr(sourceField.typ))
  
  def locationLiteral(expressionFactory : ExpressionFactory, reference : PTerm) : PTerm = {
    val fieldLiteral = expressionFactory.makeIntegerLiteralTerm(reference.sourceLocation, id)
    expressionFactory.makePDomainFunctionApplicationTerm(reference.sourceLocation,
      prelude.Pair.Location.create,PTermSequence(reference,fieldLiteral))
  }
}
