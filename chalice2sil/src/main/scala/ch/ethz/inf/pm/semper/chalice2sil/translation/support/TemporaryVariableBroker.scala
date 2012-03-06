package ch.ethz.inf.pm.semper.chalice2sil.translation.support

import silAST.programs.symbols.ProgramVariable
import silAST.types.DataType
import silAST.source.noLocation
import ch.ethz.inf.pm.semper.chalice2sil.translation.MethodEnvironment

/**
  * Author: Christian Klauser
  */

class TemporaryVariableBroker(environment : TemporaryVariableHost) {
  private val knownTemporaryVariables = collection.mutable.Set[ProgramVariable]()
  private val freeTemporaryVariables = collection.mutable.Map[DataType, List[ProgramVariable]]()

  protected def allocate(dataType : DataType) : ProgramVariable = {
    val temporary = environment.implementationFactory.addLocalVariable(noLocation, environment.getNextName("τ"), dataType)
    knownTemporaryVariables += temporary
    temporary
  }

  def acquire(dataType : DataType) : ProgramVariable = {
    val v = freeTemporaryVariables.getOrElse(dataType, Nil).headOption.getOrElse(allocate(dataType))
    environment.bringTemporaryVariableIntoScope(v)
    v
  }


  def release(variable : ProgramVariable) {
    require(knownTemporaryVariables contains variable,
      "Cannot release SIL program variable %s because it is not a temporary variable.".format(variable))
    require(!(freeTemporaryVariables(variable.dataType) contains variable),
      "Cannot release SIL program variable %s because it is not currently acquired.".format(variable))

    freeTemporaryVariables.update(variable.dataType, variable :: freeTemporaryVariables(variable.dataType))
  }

  def using(dataType : DataType, f : (ProgramVariable) => Unit) {
    val temp = acquire(dataType)
    try {
      f(temp)
    } finally {
      release(temp)
    }
  }
}