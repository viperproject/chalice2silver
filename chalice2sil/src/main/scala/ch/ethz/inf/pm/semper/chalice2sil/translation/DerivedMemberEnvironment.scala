package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.source.SourceLocation

/**
  * @author Christian Klauser
  */
class DerivedMemberEnvironment(methodEnvironment : MemberEnvironment)
  extends DerivedProgramEnvironment(methodEnvironment)
  with MemberEnvironment {

  private val parentEnvironment : MemberEnvironment = methodEnvironment match {
    case derived:DerivedMemberEnvironment => derived.parentEnvironment
    case other => other
  }
  
  assert(!parentEnvironment.isInstanceOf[DerivedMemberEnvironment])

  override def environmentReadFractionTerm(sourceLocation : SourceLocation) = parentEnvironment.environmentReadFractionTerm(sourceLocation)
  override def environmentCurrentThreadTerm(sourceLocation : SourceLocation) = parentEnvironment.environmentCurrentThreadTerm(sourceLocation)

  override def programVariables = parentEnvironment.programVariables

  override def thisVariable = parentEnvironment.thisVariable

  override def currentExpressionFactory = parentEnvironment.currentExpressionFactory

  override def nameSequence = parentEnvironment.nameSequence
}
