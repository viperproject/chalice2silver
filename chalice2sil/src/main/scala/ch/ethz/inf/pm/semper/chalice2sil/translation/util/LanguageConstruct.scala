package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import ch.ethz.inf.pm.semper.chalice2sil._
import translation._
import silAST.source.SourceLocation
import collection.TraversableOnce
import silAST.expressions.util.PTermSequence
import silAST.methods.{MethodFactory, Method}

import silAST.programs.symbols.{ProgramVariable, Field}
import silAST.expressions.terms.PTerm
import silAST.types.referenceType
import silAST.expressions.{Expression, PredicateExpression}
import silAST.symbols.logical.And


/**
  * @author Christian Klauser
  */
class LanguageConstruct(scope : ScopeTranslator, sourceLocation_ : SourceLocation)
  extends LanguageConstructBase(scope, sourceLocation_) {
  import environment._

  protected class ImpureHeapLocationOps protected[LanguageConstruct](variable : ProgramVariable, field : Field) extends PureHeapLocationOps(variable,field) {
    def <--(rhs : PTerm) { scope.currentBlock.appendFieldAssignment(variable,field,rhs)(sourceLocation) }
  }

  final implicit def heapLocationOps(location : (ProgramVariable,Field)) : ImpureHeapLocationOps = new ImpureHeapLocationOps(location._1,location._2)
  final implicit def heapLocationOpsT(location : (ProgramVariable, FieldTranslator)) : ImpureHeapLocationOps = new ImpureHeapLocationOps(location._1,location._2)

  final type MethodCallSpec = (PTerm,MethodFactory,Seq[PTerm])

  final implicit def programVariableSeqOps(vars : Seq[ProgramVariable]) = new {
    def <--(call : MethodCallSpec) {
      scope.currentBlock.appendCall(
        currentExpressionFactory.makeProgramVariableSequence(vars)(sourceLocation),
        call._1,
        call._2,
        PTermSequence(call._3:_*))(sourceLocation)
    }
    def <--(rhss : Iterable[PTerm]) {
      vars.zip(rhss).foreach(t => scope.currentBlock.appendAssignment(t._1,t._2)(sourceLocation))
    }
  }

  protected class ImpureProgramVariableOps protected[LanguageConstruct](variable : ProgramVariable)
    extends PureProgramVariableOps(variable) {
    def <--(rhs : PTerm) { scope.currentBlock.appendAssignment(variable,rhs)(sourceLocation) }
    def <--(call : MethodCallSpec) { Seq(variable) <-- call }
    def <--(newReferenceTag : NewRef) { scope.currentBlock.appendNew(variable,referenceType)(sourceLocation) }
  }

  final implicit def programVariableOps(variable : ProgramVariable) : ImpureProgramVariableOps = new ImpureProgramVariableOps(variable)
  
  final case class NewRef()

  class ImpurePTermOps(term : PTerm) extends PurePTermOps(term) {
    def !(mc : (Method,TraversableOnce[PTerm])) = (term,mc._1,mc._2)
  }
  
  final implicit def pTermOps(term : PTerm) : ImpurePTermOps = new ImpurePTermOps(term)
  
  class MethodOps protected[LanguageConstruct](method : MethodFactory) {
    def apply(args : TraversableOnce[PTerm]) = (method,args) 
  }
  
  final implicit def methodOps(method : MethodFactory) = new MethodOps(method)
  final implicit def methodOpsT(method : MethodTranslator) = new MethodOps(method.methodFactory)

  final def fold(spec : PredicateExpression) {
    scope.currentBlock.appendFold(spec)(sourceLocation)
  }

  final def unfold(spec : PredicateExpression) {
    scope.currentBlock.appendUnfold(spec)(sourceLocation)
  }

  final def inhale(expr : Expression) {
    scope.currentBlock.appendInhale(expr)(sourceLocation)
  }
  
  final def inhale(es : TraversableOnce[Expression]*) {
    es.flatten.reduceOption(currentExpressionFactory.makeBinaryExpression(And()(sourceLocation),_,_)(sourceLocation)) match {
      case Some(e) => inhale(e)
      case None => // don't append inhale
    }
  }

  final def exhale(expr : Expression) {
    scope.currentBlock.appendExhale(expr)(sourceLocation)
  }

  final def exhale(expr : Expression, message : String){
    scope.currentBlock.appendExhale(expr,Some(message))(sourceLocation)
  }

  final def exhale(es : TraversableOnce[Expression]*) {
    es.flatten.reduceOption(currentExpressionFactory.makeBinaryExpression(And()(sourceLocation),_,_)(sourceLocation)) match {
      case Some(e) => exhale(e)
      case None => // don't append inhale
    }
  }
}
