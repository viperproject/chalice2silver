package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.programs.symbols.ProgramVariable
import silAST.methods.MethodFactory
import silAST.methods.implementations.{ImplementationFactory, BasicBlockFactory}
import util._
import silAST.expressions.{Expression, ExpressionFactory}
import silAST.source.SourceLocation
import silAST.expressions.terms.Term

/**
  *
  */
trait MemberEnvironment extends ProgramEnvironment {
  def programVariables : DerivedFactoryCache[chalice.Variable,String, ProgramVariable]
  def thisVariable : ProgramVariable
  def environmentReadFractionTerm(sourceLocation : SourceLocation) : Term
  def environmentCurrentThreadTerm(sourceLocation : SourceLocation) : Term
  def currentExpressionFactory : ExpressionFactory
  def nameSequence : NameSequence
 
  //Optional
  def getNextName(prefix : String = "") = prefix match {
    case null | "" => nameSequence.nextName
    case p  => p + "_" + nameSequence.nextName
  }

  def pureLanguageConstruct[T](sourceLocation : SourceLocation)(action : PureLanguageConstruct => T) =
    action(new PureLanguageConstruct(this,sourceLocation))
}