package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.terms.Term
import silAST.expressions.Expression

/**
  * @author Christian Klauser
  */
class DefaultCodeTranslator[TExpression <: Expression,TTerm <: Term]
    (methodEnvironment : MethodEnvironment)(implicit
                                            override val expressionClassManifest : ClassManifest[TExpression],
                                            override val termClassManifest : ClassManifest[TTerm])
  extends DerivedMethodEnvironment(methodEnvironment)
  with ExpressionTranslator[TExpression]
  with TermTranslator[TTerm] 
  with PermissionTranslator[TTerm] {

}
