package ch.ethz.inf.pm.semper.chalice2sil.translation

import semper.sil.ast.expressions.terms.Term
import semper.sil.ast.expressions.Expression
import collection.mutable
import semper.sil.ast.symbols.logical.quantification.LogicalVariable

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
