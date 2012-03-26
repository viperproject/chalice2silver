package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.terms.Term
import silAST.expressions.Expression

/**
  * @author Christian Klauser
  */
class DefaultCodeTranslator
    (methodEnvironment : MemberEnvironment)
  extends DerivedMemberEnvironment(methodEnvironment)
  with ExpressionTranslator
  with TermTranslator
  with PermissionTranslator
