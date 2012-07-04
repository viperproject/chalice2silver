package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.source.SourceLocation

/**
  * @author Christian Klauser
  */
class DerivedMemberEnvironment(memberEnvironment : MemberEnvironment)
  extends DerivedProgramEnvironment(memberEnvironment)
  with MemberEnvironment {

  override def environmentReadFractionTerm(sourceLocation : SourceLocation) = memberEnvironment.environmentReadFractionTerm(sourceLocation)
  override def environmentCurrentThreadTerm(sourceLocation : SourceLocation) = memberEnvironment.environmentCurrentThreadTerm(sourceLocation)

  override def programVariables = memberEnvironment.programVariables

  override def thisVariable = memberEnvironment.thisVariable

  override def currentExpressionFactory = memberEnvironment.currentExpressionFactory

  override def nameSequence = memberEnvironment.nameSequence
}
