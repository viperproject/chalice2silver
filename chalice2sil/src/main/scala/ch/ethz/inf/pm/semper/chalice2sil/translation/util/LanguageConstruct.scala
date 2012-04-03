package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import ch.ethz.inf.pm.semper.chalice2sil._
import translation._
import silAST.source.SourceLocation
import collection.TraversableOnce
import silAST.expressions.util.PTermSequence
import silAST.methods.{MethodFactory, Method}

import silAST.programs.symbols.{PredicateFactory, ProgramVariable, Field}
import silAST.expressions.terms.{Term, PTerm}
import silAST.types.referenceType
import silAST.expressions.{Expression, PredicateExpression}
import silAST.symbols.logical.And


/**
  * @author Christian Klauser
  */
class LanguageConstruct(environment : ScopeTranslator, sourceLocation_ : SourceLocation)
  extends LanguageConstructBase(environment, sourceLocation_) {
  import environment._

  protected class ImpureHeapLocationOps protected[LanguageConstruct](variable : ProgramVariable, field : Field) extends PureHeapLocationOps(variable,field) {
    def <--(rhs : PTerm) { environment.currentBlock.appendFieldAssignment(sourceLocation,variable,field,rhs) }
  }

  final implicit def heapLocationOps(location : (ProgramVariable,Field)) : ImpureHeapLocationOps = new ImpureHeapLocationOps(location._1,location._2)
  final implicit def heapLocationOpsT(location : (ProgramVariable, FieldTranslator)) : ImpureHeapLocationOps = new ImpureHeapLocationOps(location._1,location._2)

  final type MethodCallSpec = (PTerm,MethodFactory,Seq[PTerm])

  final implicit def programVariableSeqOps(vars : Seq[ProgramVariable]) = new {
    def <--(call : MethodCallSpec) {
      environment.currentBlock.appendCall(sourceLocation,
        currentExpressionFactory.makeProgramVariableSequence(sourceLocation, vars),
        call._1,
        call._2,
        PTermSequence(call._3:_*))
    }
    def <--(rhss : Iterable[PTerm]) {
      vars.zip(rhss).foreach(t => environment.currentBlock.appendAssignment(sourceLocation, t._1,t._2))
    }
  }

  protected class ImpureProgramVariableOps protected[LanguageConstruct](variable : ProgramVariable)
    extends PureProgramVariableOps(variable) {
    def <--(rhs : PTerm) { environment.currentBlock.appendAssignment(sourceLocation, variable,rhs) }
    def <--(call : MethodCallSpec) { Seq(variable) <-- call }
    def <--(newReferenceTag : NewRef) { environment.currentBlock.appendNew(sourceLocation,variable,referenceType) }
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
    environment.currentBlock.appendFold(sourceLocation,spec)
  }

  final def unfold(spec : PredicateExpression) {
    environment.currentBlock.appendUnfold(sourceLocation, spec)
  }

  final def inhale(expr : Expression) {
    environment.currentBlock.appendInhale(sourceLocation, expr)
  }
  
  final def inhale(es : TraversableOnce[Expression]*) {
    es.flatten.reduceOption(currentExpressionFactory.makeBinaryExpression(sourceLocation, And()(sourceLocation),_,_)) match {
      case Some(e) => inhale(e)
      case None => // don't append inhale
    }
  }

  final def exhale(expr : Expression) {
    environment.currentBlock.appendExhale(sourceLocation, expr)
  }

  final def exhale(es : TraversableOnce[Expression]*) {
    es.flatten.reduceOption(currentExpressionFactory.makeBinaryExpression(sourceLocation, And()(sourceLocation),_,_)) match {
      case Some(e) => exhale(e)
      case None => // don't append inhale
    }
  }
}
