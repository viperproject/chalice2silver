package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.programs.symbols.ProgramVariable
import silAST.methods.MethodFactory
import silAST.methods.implementations.{ImplementationFactory, BasicBlockFactory}
import silAST.expressions.ExpressionFactory
import util._

/**
  *
  */
trait MethodEnvironment extends ProgramEnvironment {
  def programVariables : DerivedFactoryCache[chalice.Variable,String, ProgramVariable]
  def thisVariable : ProgramVariable
  def readFractionVariable : ProgramVariable
  def currentExpressionFactory : ExpressionFactory
  def nameSequence : NameSequence
 
  //Optional
  def getNextName(prefix : String = "") = prefix match {
    case null | "" => nameSequence.nextName
    case p  => p + "_" + nameSequence.nextName
  }
}