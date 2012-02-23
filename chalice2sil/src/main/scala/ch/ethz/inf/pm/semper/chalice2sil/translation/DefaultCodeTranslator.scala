package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.terms.Term
import silAST.expressions.Expression

/**
  * @author Christian Klauser
  */
class DefaultCodeTranslator
    (methodEnvironment : MethodEnvironment)
  extends DerivedMethodEnvironment(methodEnvironment)
  with ExpressionTranslator
  with TermTranslator
  with PermissionTranslator
