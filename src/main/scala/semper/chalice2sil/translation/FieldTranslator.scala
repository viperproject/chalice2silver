package semper.chalice2sil.translation

import semper.sil.ast.expressions.ExpressionFactory
import semper.sil.ast.expressions.terms.PTerm
import semper.sil.ast.expressions.util.PTermSequence
import semper.sil.ast.programs.symbols.Field

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
  with LocationTranslator
  with TypeTranslator {


  override def toString = field.toString()
}
