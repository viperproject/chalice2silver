package semper.chalice2sil.translation.util

import semper.sil.ast.source.SourceLocation
import semper.sil.ast.programs.symbols.{Field, ProgramVariable}
import semper.chalice2sil.translation.{FieldTranslator, MemberEnvironment}
import semper.chalice2sil._
import semper.sil.ast.expressions._
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
