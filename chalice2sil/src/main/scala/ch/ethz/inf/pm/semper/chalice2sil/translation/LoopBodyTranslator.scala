package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.source.SourceLocation
import silAST.types.DataType
import collection._
import silAST.methods.implementations.{LoopBlockFactory, BasicBlockFactory}
import silAST.expressions.ExpressionFactory
import silAST.programs.symbols.ProgramVariable

/**
  * @author Christian Klauser
  * @param environmentReadFractionVariable The read fraction specific to this loop body.
  */
class LoopBodyTranslator(
                          environment : ScopeTranslator,
                          protected val loopBlockFactory : LoopBlockFactory,
                          override protected val environmentCurrentThreadVariable : ProgramVariable,
                          override protected val environmentReadFractionVariable : ProgramVariable)
  extends DerivedMemberEnvironment(environment)
  with ScopeTranslator {

  def cfgFactory = loopBlockFactory.bodyFactory

  val blockStack = new mutable.Stack[BasicBlockFactory]
  override def currentExpressionFactory : ExpressionFactory = blockStack.headOption.getOrElse(loopBlockFactory)

  def temporaries = environment.temporaries

  def declareScopedVariable(sourceLocation : SourceLocation, uniqueName : String, dataType : DataType) =
    loopBlockFactory.addProgramVariable(uniqueName,dataType)(sourceLocation)
}
