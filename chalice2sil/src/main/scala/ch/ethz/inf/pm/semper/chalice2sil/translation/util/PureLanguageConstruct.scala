package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import silAST.source.SourceLocation
import silAST.programs.symbols.{Field, ProgramVariable}
import ch.ethz.inf.pm.semper.chalice2sil.translation.{FieldTranslator, MemberEnvironment}
import ch.ethz.inf.pm.semper.chalice2sil._
import silAST.expressions._
import terms._


/**
  * @author Christian Klauser
  */
final class PureLanguageConstruct(environment : MemberEnvironment, sourceLocation : SourceLocation)
  extends LanguageConstructBase(environment, sourceLocation) {

  implicit def programVariableOps(variable : ProgramVariable) : PureProgramVariableOps =
    new PureProgramVariableOps(variable)

  implicit def heapLocationOps(location : (ProgramVariable,Field)) : PureHeapLocationOps = new PureHeapLocationOps(location._1,location._2)
  implicit def heapLocationOpsT(location : (ProgramVariable, FieldTranslator)) : PureHeapLocationOps = new PureHeapLocationOps(location._1,location._2)

  implicit def pTermOps(term : PTerm) : PurePTermOps = new PurePTermOps(term)
}
