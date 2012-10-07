package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.terms.Term
import silAST.expressions.Expression
import collection.mutable
import silAST.symbols.logical.quantification.LogicalVariable

/**
  * @author Christian Klauser
  */
class DefaultCodeTranslator
    (methodEnvironment : MemberEnvironment)
  extends DerivedMemberEnvironment(methodEnvironment)
  with ExpressionTranslator
  with TermTranslator
  with PermissionTranslator
{
  protected override val quantifierScopes : mutable.Stack[Map[String,LogicalVariable]] = mutable.Stack()
}
