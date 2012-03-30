package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import silAST.source.SourceLocation
import silAST.programs.symbols.{PredicateFactory, Field, ProgramVariable}
import ch.ethz.inf.pm.semper.chalice2sil.translation.{PredicateTranslator, FieldTranslator, DerivedMemberEnvironment, MemberEnvironment}
import ch.ethz.inf.pm.semper.chalice2sil._
import silAST.expressions._
import silAST.expressions.PExpression
import terms._
import silAST.expressions.util.{GTermSequence, PTermSequence, TermSequence}
import silAST.domains.{DomainFunction, DomainPredicate}
import silAST.symbols.logical.{UnaryConnective, BinaryConnective, And}



/**
  * @author Christian Klauser
  */
final class PureLanguageConstruct(environment : MemberEnvironment, sourceLocation : SourceLocation)
  extends LanguageConstructBase(environment, sourceLocation) {
  import environment._

  implicit def programVariableOps(variable : ProgramVariable) : PureProgramVariableOps =
    new PureProgramVariableOps(variable)

  implicit def heapLocationOps(location : (ProgramVariable,Field)) : PureHeapLocationOps = new PureHeapLocationOps(location._1,location._2)
  implicit def heapLocationOpsT(location : (ProgramVariable, FieldTranslator)) : PureHeapLocationOps = new PureHeapLocationOps(location._1,location._2)

  implicit def pTermOps(term : PTerm) : PurePTermOps = new PurePTermOps(term)
}
