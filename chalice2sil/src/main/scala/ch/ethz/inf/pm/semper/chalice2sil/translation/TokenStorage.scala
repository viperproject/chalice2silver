package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.OldExpression

/**
  * @author Christian Klauser
  */
class TokenStorage(val receiver : FieldTranslator, 
                   val args : List[FieldTranslator], 
                   val readFraction : FieldTranslator, 
                   val oldTerms : Map[OldExpression,FieldTranslator]) extends Iterable[FieldTranslator] {

  val allFields : List[FieldTranslator] = receiver :: (args ++ (readFraction :: oldTerms.values.toList))

  def iterator = allFields.iterator
}

object TokenStorage {
  //def fromMethod(method : MethodTranslator) : TokenStorage = {  }
}
