package ch.ethz.inf.pm.semper.chalice2sil.translation.support

import ch.ethz.inf.pm.semper.chalice2sil.translation.MethodEnvironment
import silAST.programs.symbols.ProgramVariable

/**
  * @author Christian Klauser
  */
trait TemporaryVariableHost extends MethodEnvironment {
  def bringTemporaryVariableIntoScope(v : ProgramVariable)
}
