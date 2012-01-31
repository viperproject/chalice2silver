package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.programs.symbols.ProgramVariable
import silAST.types.DataType
import silAST.source.noLocation

/**
 * Author: Christian Klauser
 */

class TemporaryVariableBroker(environment : MethodEnvironment) {
  private val knownTemporaryVariables = collection.mutable.Set[ProgramVariable]()
  private val freeTemporaryVariables = collection.mutable.Map[DataType, List[ProgramVariable]]()

  protected def allocate(dataType : DataType) : ProgramVariable = {
    val temporary = environment.implementationFactory.addLocalVariable(noLocation,environment.getNextName("τ"),dataType)
    knownTemporaryVariables += temporary
    temporary
  }

  def acquire(dataType : DataType) : ProgramVariable =
    freeTemporaryVariables.getOrElse(dataType,Nil).headOption.getOrElse(allocate(dataType))

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