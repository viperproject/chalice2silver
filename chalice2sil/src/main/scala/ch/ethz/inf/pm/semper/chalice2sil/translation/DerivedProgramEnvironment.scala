package ch.ethz.inf.pm.semper.chalice2sil.translation

/**
  * Implements the [[ch.ethz.inf.pm.semper.chalice2sil.translation.ProgramEnvironment]] trait by forwarding
  * all method calls to another program environment instance.
  * @author Christian Klauser
  */
class DerivedProgramEnvironment(programEnvironment: ProgramEnvironment) extends ProgramEnvironment {
  def programFactory = programEnvironment.programFactory

  def methodFactories = programEnvironment.methodFactories

  def fields = programEnvironment.fields

  def prelude = programEnvironment.prelude

  def programOptions = programEnvironment.programOptions

  def onNewMessage = programEnvironment.onNewMessage

  def pastMessages = programEnvironment.pastMessages

  def predicates = programEnvironment.predicates

  def functions = programEnvironment.functions
}
