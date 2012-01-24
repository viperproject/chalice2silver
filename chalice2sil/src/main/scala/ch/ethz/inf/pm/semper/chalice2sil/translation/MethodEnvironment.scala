package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.programs.symbols.ProgramVariable
import silAST.methods.MethodFactory
import silAST.methods.implementations.{ImplementationFactory, BasicBlockFactory}

/**
 * Author: Christian Klauser
 * Date: 24.01.12
 */

trait MethodEnvironment extends ProgramEnvironment {
  def implementationFactory : ImplementationFactory
  def localVariables : (chalice.Variable => ProgramVariable)
  def basicBlocks : (String => BasicBlockFactory)
 
  //Optional
  protected def nameSequence = NameSequence()
  def getNextName(prefix : String = "") = prefix match {
    case "" => nameSequence.nextName
    case p  => p + "_" + nameSequence.nextName
  }
}