package semper.chalice2sil.translation

import semper.sil.ast.source.SourceLocation

/**
  * @author Christian Klauser
  */
class DerivedMemberEnvironment(memberEnvironment : MemberEnvironment)
  extends DerivedProgramEnvironment(memberEnvironment)
  with MemberEnvironment {

  override def environmentReadFractionExpression(sourceLocation : SourceLocation) = memberEnvironment.environmentReadFractionExpression(sourceLocation)
  override def environmentCurrentThreadExpression(sourceLocation : SourceLocation) = memberEnvironment.environmentCurrentThreadExpression(sourceLocation)

  override def programVariables = memberEnvironment.programVariables

  override def thisVariable = memberEnvironment.thisVariable

  override def currentExpressionFactory = memberEnvironment.currentExpressionFactory

  override def nameSequence = memberEnvironment.nameSequence
}
