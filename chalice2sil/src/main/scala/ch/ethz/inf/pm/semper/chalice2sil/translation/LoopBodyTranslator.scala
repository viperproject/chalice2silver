package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.source.SourceLocation
import silAST.types.DataType
import collection._
import silAST.methods.implementations.{LoopBlockFactory, BasicBlockFactory}
import silAST.expressions.ExpressionFactory
import silAST.programs.symbols.ProgramVariable

/**
  * @author Christian Klauser
  */
class LoopBodyTranslator(environment : ScopeTranslator, protected val loopBlockFactory : LoopBlockFactory, val environmentCurrentThreadVariable : ProgramVariable)
  extends DerivedMemberEnvironment(environment)
  with ScopeTranslator {

  def cfgFactory = loopBlockFactory.bodyFactory

  val blockStack = new mutable.Stack[BasicBlockFactory]
  override def currentExpressionFactory : ExpressionFactory = blockStack.headOption.getOrElse(loopBlockFactory)

  def temporaries = environment.temporaries

  def declareScopedVariable(sourceLocation : SourceLocation, uniqueName : String, dataType : DataType) =
    loopBlockFactory.addProgramVariable(uniqueName,dataType)(sourceLocation)
}
