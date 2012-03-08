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
    val v = freeTemporaryVariables.getOrElseUpdate(dataType, Nil).headOption match {
      case None => allocate(dataType)
      case Some(e) =>
        freeTemporaryVariables.update(dataType,freeTemporaryVariables(dataType).tail)
        e
    }
    environment.bringTemporaryVariableIntoScope(v)
    v
  }


  def release(variable : ProgramVariable) {
    require(knownTemporaryVariables contains variable,
      "Cannot release SIL program variable %s because it is not a temporary variable.".format(variable))
    require((freeTemporaryVariables.get(variable.dataType).map(vs => ! (vs contains variable)).getOrElse(true)),
      "Cannot release SIL program variable %s because it is not currently acquired.".format(variable))

    val oldList = freeTemporaryVariables.getOrElse(variable.dataType,Nil)
    freeTemporaryVariables.update(variable.dataType, variable :: oldList)
  }

  def using[T](dataType : DataType)(f : (ProgramVariable) => T) : T = {
    val temp = acquire(dataType)
    try {
      f(temp)
    } finally {
      release(temp)
    }
  }
}