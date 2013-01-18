package semper.chalice2sil.translation


/**
  * @author Christian Klauser
  */
class TokenStorage(programEnvironment : ProgramEnvironment,
                    val args : List[FieldTranslator],
                    val results : List[FieldTranslator],
                    val oldExpressions : Map[OldNode,FieldTranslator])
  extends DerivedProgramEnvironment(programEnvironment)
  with Iterable[FieldTranslator] {

  val receiver = args.head
  val readFraction = args.drop(args.length-2).head // known to exist because all methods have at least 3 arguments
  val allFields : List[FieldTranslator] = prelude.Token.joinable :: (args ++ oldExpressions.values.toList)

  def iterator = allFields.iterator
}
