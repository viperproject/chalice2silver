/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.chalice2sil.translation

import viper.silver.ast._
import viper.silver.ast.utility.Consistency


/**
 * A helper class for managing Silver method's local variables.
 * @param silMethod
 */
class MethodVariableRegistry(val silMethod: Method) {

  private def constructVariableFullName(variableName: String, variableType: Type): String = {
    s"${variableName}$$${variableType.toString()}"
  }

  private def isFormalArgument(variableName: String): Boolean = {
    silMethod.formalArgs.exists(_.name == variableName)
  }

  private def isFormalReturn(variableName: String): Boolean = {
    silMethod.formalReturns.exists(_.name == variableName)
  }

  private def isLocalVariable(variableName: String): Boolean = {
    silMethod.locals.exists(_.name == variableName)
  }

  private def getFormalArgument(variableName: String, variableType: Type): Option[LocalVar] = {
    silMethod.formalArgs.find(_.name == variableName) match {
      case Some(variable) =>
        assert(variable.typ == variableType)
        Some(variable.localVar)
      case _ =>
        None
    }
  }

  private def getFormalReturn(variableName: String, variableType: Type): Option[LocalVar] = {
    silMethod.formalReturns.find(_.name == variableName) match {
      case Some(variable) =>
        assert(variable.typ == variableType)
        Some(variable.localVar)
      case _ =>
        None
    }
  }

  /**
   * Returns the object representing a locally declared variable.
   *
   * In case of failure, true indicates that the variable with such name
   * was found, but its type did not math. False means that no variable
   * with such name were found at all.
   */
  private def getLocalVariable(variableName: String, variableType: Type): Either[Boolean, LocalVar] = {
    val fullVariableName = constructVariableFullName(variableName, variableType)
    silMethod.locals.find(_.name == fullVariableName) match {
      case Some(variable) =>
        assert(variable.typ == variableType)
        Right(variable.localVar)
      case _ =>
        silMethod.locals.find(_.name == variableName) match {
          case Some(variable) =>
            if (variable.typ == variableType)
              Right(variable.localVar)
            else
              Left(true)
          case _ =>
            Left(false)
        }
    }
  }

  private def createLocalVarDecl(variableName: String, variableType: Type, position: SourcePosition) = {
    assert(!isFormalArgument(variableName),
      s"Local variable name ${variableName} collides with formal argument name.")
    assert(!isFormalReturn(variableName),
      s"Local variable name ${variableName} collides with formal return name.")
    assert(!isLocalVariable(variableName),
      s"Local variable name ${variableName} collides with existing local variable name.")
    val variable = LocalVarDecl(variableName, variableType)(position)
    silMethod.locals = silMethod.locals :+ variable
    variable
  }

  /**
   * Returns variable object for use in expressions.
   */
  def getVariable(chaliceVariableName: String, variableType: Type): Option[LocalVar] = {
    val variableName = Util.toValidSilverIdentifier(chaliceVariableName)
    getFormalArgument(variableName, variableType) match {
      case Some(variable) =>
        Some(variable)
      case _ =>
        getFormalReturn(variableName, variableType) match {
          case Some(variable) =>
            Some(variable)
          case _ =>
            getLocalVariable(variableName, variableType) match {
              case Right(variable) =>
                Some(variable)
              case _ =>
                None
            }
        }
    }
  }

  def getVariable(chaliceVariable: chalice.Variable): LocalVar = {
    val variableName = chaliceVariable.id
    val variableType = Util.translateType(chaliceVariable.t)
    getVariable(variableName, variableType) match {
      case Some(variable) =>
        variable
      case _ =>
        throw new NoSuchElementException(s"Variable ${variableName}:${variableType} was not found.")
    }
  }

  def getRefVariable(variableName: String): LocalVar = {
    getVariable(variableName, Ref) match {
      case Some(variable) =>
        variable
      case _ =>
        throw new NoSuchElementException(s"Variable ${variableName}:${Ref} was not found.")
    }
  }

  def getVariableOrThrow(variableName: String, variableType: Type): LocalVar = {
    getVariable(variableName, variableType) match {
      case Some(variable) =>
        variable
      case _ =>
        throw new NoSuchElementException(s"Variable ${variableName}:${variableType} was not found.")
    }
  }

  /**
   * Method for getting token variables for expressions
   * fork tk := ?
   * Creates a new variable if needed.
   */
  def getOrCreateTokenVariable(variableName: String, position: SourcePosition): LocalVar = {
    assert(!isFormalArgument(variableName),
      s"Local variable name ${variableName} collides with formal argument name.")
    getFormalReturn(variableName, Ref) match {
      case Some(variable) =>
        variable
      case _ =>
        getLocalVariable(variableName, Ref) match {
          case Right(variable) =>
            variable
          case Left(true) =>
            // Variable name collision, use full name.
            createLocalVarDecl(
              constructVariableFullName(variableName, Ref),
              Ref,
              position).localVar
          case Left(false) =>
            // No variable with such name exists.
            createLocalVarDecl(
              variableName,
              Ref,
              position).localVar
        }
    }
  }

  private def addLocalVarDecl(chaliceVariableName: String, variableType: Type, position: SourcePosition): Unit = {
    val variableName = Util.toValidSilverIdentifier(chaliceVariableName)

    val fullVariableName = constructVariableFullName(variableName, variableType)
    assert(!isFormalArgument(variableName),
      s"Local variable name ${variableName} collides with formal argument name.")
    assert(!isFormalReturn(variableName),
      s"Local variable name ${variableName} collides with formal return name.")

    silMethod.locals.find((v) => v.name == variableName) match {
      case Some(silverVariable) =>
        // There already exists variable with this name.
        if (silverVariable.typ == variableType) {
          // We have a declaration with the same name and type. It is safe to reuse it.
        } else {
          // The same variable name, but different types. We need to rename.
          createLocalVarDecl(fullVariableName, variableType, position)
        }
      case _ =>
        // Variable with this name does not exist yet. Just create one.
        createLocalVarDecl(variableName, variableType, position)
    }
  }

  /**
   * Add new explicit local variable declaration.
   * @param chaliceVariable Chalice local variable
   * @param position position of Chalice local variable declaration
   */
  def addLocalVarDecl(chaliceVariable: chalice.Variable, position: SourcePosition): Unit = {
    val variableName = chaliceVariable.id
    val variableType = Util.translateType(chaliceVariable.t)
    addLocalVarDecl(variableName, variableType, position)
  }

  /**
   * Add new explicit local reference variable declaration, which is used to translate
   * var x := new X
   * @param variableName
   * @param position
   */
  def addReferenceLocalVarDecl(variableName: String, position: SourcePosition): Unit = {
    addLocalVarDecl(variableName, Ref, position)
  }

}

