package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.programs.symbols.ProgramVariable
import silAST.methods.MethodFactory
import silAST.methods.implementations.{ImplementationFactory, BasicBlockFactory}
import silAST.expressions.ExpressionFactory
import support.TemporaryVariableBroker

/**
 * Author: Christian Klauser
 * Date: 24.01.12
 */

trait MethodEnvironment extends ProgramEnvironment {
  def implementationFactory : ImplementationFactory
  def programVariables : DerivedFactoryCache[chalice.Variable,String, ProgramVariable]
  def basicBlocks : FactoryCache[String,  BasicBlockFactory] with AdjustableCache[BasicBlockFactory]
  def temporaries : TemporaryVariableBroker
  def thisVariable : ProgramVariable
  def currentExpressionFactory : ExpressionFactory
  def nameSequence : NameSequence
 
  //Optional
  def getNextName(prefix : String = "") = prefix match {
    case "" => nameSequence.nextName
    case p  => p + "_" + nameSequence.nextName
  }
}