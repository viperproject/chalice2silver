package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.source.SourceLocation
import silAST.types.DataType
import collection.mutable.Stack
import silAST.methods.implementations.{LoopBlockFactory, BasicBlockFactory}

/**
  * @author Christian Klauser
  */
class LoopBodyTranslator(environment : ScopeTranslator, protected val loopBlockFactory : LoopBlockFactory)
  extends DerivedMemberEnvironment(environment)
  with ScopeTranslator {

  def cfgFactory = loopBlockFactory.bodyFactory

  val blockStack = new Stack[BasicBlockFactory]

  def temporaries = environment.temporaries

  def declareScopedVariable(sourceLocation : SourceLocation, uniqueName : String, dataType : DataType) =
    loopBlockFactory.addProgramVariable(uniqueName,dataType)(sourceLocation)
}
