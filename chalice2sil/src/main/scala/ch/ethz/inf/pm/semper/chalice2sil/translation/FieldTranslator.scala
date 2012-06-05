package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.ExpressionFactory
import silAST.expressions.terms.PTerm
import silAST.expressions.util.PTermSequence
import silAST.programs.symbols.Field

/**
  * @author Christian Klauser
  */
class FieldTranslator(
                       /**
                         * The SILAST field
                         */
                       val field : Field,

                       /**
                         * An id that is unique within the program and is used to identify this field
                         * in expressions.
                         */
                       val id : Int, programEnvironment : ProgramEnvironment)
  extends DerivedProgramEnvironment(programEnvironment)
  with TypeTranslator {
  
  def locationLiteral(expressionFactory : ExpressionFactory, reference : PTerm) : PTerm = {
    val fieldLiteral = expressionFactory.makeIntegerLiteralTerm(id,reference.sourceLocation)
    expressionFactory.makePDomainFunctionApplicationTerm(
      prelude.Pair.Location.create,PTermSequence(reference,fieldLiteral),reference.sourceLocation)
  }

  override def toString = field.toString;
}
