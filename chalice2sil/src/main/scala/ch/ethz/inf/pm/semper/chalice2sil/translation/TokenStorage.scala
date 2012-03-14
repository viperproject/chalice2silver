package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.OldExpression

/**
  * @author Christian Klauser
  */
class TokenStorage(programEnvironment : ProgramEnvironment,
                    val args : List[FieldTranslator],
                   val oldTerms : Map[OldNode,FieldTranslator])
  extends DerivedProgramEnvironment(programEnvironment)
  with Iterable[FieldTranslator] {

  val receiver = programEnvironment.prelude.Token.receiverField
  val readFraction = programEnvironment.prelude.Token.readFractionField
  val allFields : List[FieldTranslator] = receiver :: (args ++ (readFraction :: oldTerms.values.toList))

  def iterator = allFields.iterator
}
