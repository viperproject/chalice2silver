package ch.ethz.inf.pm.semper.chalice2sil.translation

/**
  * @author Christian Klauser
  */
class DerivedMethodEnvironment(methodEnvironment : MethodEnvironment)
  extends DerivedProgramEnvironment(methodEnvironment)
  with MethodEnvironment {

  private val parentEnvironment : MethodEnvironment = methodEnvironment match {
    case derived:DerivedMethodEnvironment => derived.parentEnvironment
    case other => other
  }
  
  assert(!parentEnvironment.isInstanceOf[DerivedMethodEnvironment])

  override def implementationFactory = parentEnvironment.implementationFactory

  override def programVariables = parentEnvironment.programVariables

  override def basicBlocks = parentEnvironment.basicBlocks

  override def temporaries = parentEnvironment.temporaries

  override def thisVariable = parentEnvironment.thisVariable

  override def currentExpressionFactory = parentEnvironment.currentExpressionFactory
  
  override def localVariableVersion(v : chalice.Variable) = parentEnvironment.localVariableVersion(v)

  override def nameSequence = parentEnvironment.nameSequence
}
