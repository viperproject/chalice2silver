package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.OldExpression

/**
  * @author Christian Klauser
  */
class TokenStorage(programEnvironment : ProgramEnvironment,
                    val args : List[FieldTranslator],
                    val results : List[FieldTranslator],
                    val oldTerms : Map[OldNode,FieldTranslator])
  extends DerivedProgramEnvironment(programEnvironment)
  with Iterable[FieldTranslator] {

  val receiver = args.head
  val readFraction = args.drop(args.length-2).head // known to exist because all methods have at least 3 arguments
  val allFields : List[FieldTranslator] = prelude.Token.joinable :: (args ++ oldTerms.values.toList)

  def iterator = allFields.iterator
}
