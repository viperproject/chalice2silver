package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import semper.sil.ast.programs.symbols.ProgramVariable
import semper.sil.ast.types.DataType
import semper.sil.ast.source.noLocation
import ch.ethz.inf.pm.semper.chalice2sil.translation.ScopeTranslator

/**
  * Author: Christian Klauser
  */

class TemporaryVariableBroker(environment : ScopeTranslator) {
  private val knownTemporaryVariables = collection.mutable.Set[ProgramVariable]()
  private val freeTemporaryVariables = collection.mutable.Map[DataType, List[ProgramVariable]]()

  protected def allocate(dataType : DataType) : ProgramVariable = {
    val temporary = environment.declareScopedVariable(noLocation, environment.getNextName("t"),dataType)
    knownTemporaryVariables += temporary
    temporary
  }

  def acquire(dataType : DataType) : ProgramVariable = {
    freeTemporaryVariables.getOrElseUpdate(dataType, Nil).headOption match {
      case None => allocate(dataType)
      case Some(e) =>
        freeTemporaryVariables.update(dataType, freeTemporaryVariables(dataType).tail)
        e
    }
  }

  def isTemporary(variable : ProgramVariable) : Boolean = knownTemporaryVariables contains variable

  def release(variable : ProgramVariable) {
    require(knownTemporaryVariables contains variable,
      "Cannot release SIL program variable %s because it is not a temporary variable.".format(variable))
    require((freeTemporaryVariables.get(variable.dataType).map(vs => !(vs contains variable)).getOrElse(true)),
      "Cannot release SIL program variable %s because it is not currently acquired.".format(variable))

    val oldList = freeTemporaryVariables.getOrElse(variable.dataType, Nil)
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

  def using[T](dataType1 : DataType, dataType2 : DataType)(f : (ProgramVariable,ProgramVariable) => T) : T = {
    val temp1 = acquire(dataType1)
    val temp2 = acquire(dataType2)
    try {
      f(temp1,temp2)
    } finally {
      release(temp1)
      release(temp2)
    }
  }
}