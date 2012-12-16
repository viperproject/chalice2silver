package semper.chalice2sil.translation

import semper.sil.ast.source.SourceLocation
import semper.sil.ast.types.DataType
import collection._
import semper.sil.ast.methods.implementations.{LoopBlockFactory, BasicBlockFactory}
import semper.sil.ast.expressions.ExpressionFactory
import semper.sil.ast.programs.symbols.ProgramVariable

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
