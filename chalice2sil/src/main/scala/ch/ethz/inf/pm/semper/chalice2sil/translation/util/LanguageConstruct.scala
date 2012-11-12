package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import ch.ethz.inf.pm.semper.chalice2sil._
import translation._
import silAST.source.SourceLocation
import collection.TraversableOnce
import silAST.expressions.util.PTermSequence
import silAST.methods.{MethodFactory, Method}

import silAST.types.referenceType
import silAST.expressions.Expression
import silAST.symbols.logical.And
import silAST.programs.symbols.{PredicateFactory, ProgramVariable, Field}
import silAST.expressions.terms.{Term, PTerm}


/**
  * @author Christian Klauser
  */
class LanguageConstruct(scope : ScopeTranslator, sourceLocation_ : SourceLocation)
  extends LanguageConstructBase(scope, sourceLocation_) {
  import environment._

  protected var nextComment : List[String] = Nil
  protected final def takeComment() : List[String] = {
    val r = nextComment.reverse
    nextComment = Nil
    r
  }

  final def comment(c : String) {
    nextComment = c :: nextComment
  }

  protected class ImpureHeapLocationOps protected[LanguageConstruct](variable : ProgramVariable, field : Field) extends PureHeapLocationOps(variable,field) {
    def <--(rhs : PTerm) { scope.currentBlock.appendFieldAssignment(variable,field,rhs,sourceLocation,takeComment()) }
    def <--(rhs : (ProgramVariable, Field)) {
      this <-- scope.currentExpressionFactory.makePFieldReadTerm(
        scope.currentExpressionFactory.makeProgramVariableTerm(rhs._1,sourceLocation),
        rhs._2,
        sourceLocation) }
  }

  final implicit def heapLocationOps(location : (ProgramVariable,Field)) : ImpureHeapLocationOps = new ImpureHeapLocationOps(location._1,location._2)
  final implicit def heapLocationOpsT(location : (ProgramVariable, FieldTranslator)) : ImpureHeapLocationOps = new ImpureHeapLocationOps(location._1,location._2)

  final type MethodCallSpec = (PTerm,MethodFactory,Seq[PTerm])

  final implicit def programVariableSeqOps(vars : Seq[ProgramVariable]) = new {
    def <--(call : MethodCallSpec) {
      scope.currentBlock.appendCall(
        currentExpressionFactory.makeProgramVariableSequence(vars,sourceLocation),
        call._1,
        call._2,
        PTermSequence(call._3:_*),sourceLocation,takeComment())
    }
    def <--(rhss : Iterable[PTerm]) {
      vars.zip(rhss).foreach(t => scope.currentBlock.appendAssignment(t._1,t._2,sourceLocation,takeComment()))
    }
  }

  protected class ImpureProgramVariableOps protected[LanguageConstruct](variable : ProgramVariable)
    extends PureProgramVariableOps(variable) {
    def <--(rhs : PTerm) { scope.currentBlock.appendAssignment(variable,rhs,sourceLocation,takeComment()) }
    def <--(call : MethodCallSpec) { Seq(variable) <-- call }
    def <--(newReferenceTag : NewRef) { scope.currentBlock.appendNew(variable,referenceType,sourceLocation,takeComment()) }
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

  final def fold(receiver : Term, predicate : PredicateFactory,permission : Term) {
    scope.currentBlock.appendFold(receiver,predicate,permission,sourceLocation,takeComment())
  }

  final def unfold(receiver : Term, predicate : PredicateFactory,permission : Term) {
    scope.currentBlock.appendUnfold(
      currentExpressionFactory.makePredicatePermissionExpression(receiver,predicate,permission,sourceLocation),
      sourceLocation,
      takeComment())
  }

  final def inhale(expr : Expression) {
    scope.currentBlock.appendInhale(expr,sourceLocation,takeComment())
  }

  final def inhale(expr : Expression, customSourceLocation : SourceLocation) {
    scope.currentBlock.appendInhale(expr,customSourceLocation,takeComment())
  }
  
  final def inhale(es : Expression*) {
    es.reduceOption(currentExpressionFactory.makeBinaryExpression(And()(sourceLocation),_,_,sourceLocation)) match {
      case Some(e) => inhale(e)
      case None => // don't append inhale
    }
  }

  final def exhale(expr : Expression) {
    scope.currentBlock.appendExhale(expr,None,sourceLocation,takeComment())
  }

  final def exhale(expr : Expression, message : String){
    scope.currentBlock.appendExhale(expr,Some(message),sourceLocation,takeComment())
  }

  final def exhale(expr : Expression, message : String, customSourceLocation : SourceLocation){
    scope.currentBlock.appendExhale(expr,Some(message),customSourceLocation,takeComment())
  }

  final def exhale(es : TraversableOnce[Expression]*) {

  }
  final def exhale(es : TraversableOnce[Expression], message : Option[String] = None, customSourceLocation : Option[SourceLocation] = None) {
    es.reduceOption(currentExpressionFactory.makeBinaryExpression(And()(sourceLocation),_,_,sourceLocation)) match {
      case Some(e) => scope.currentBlock.appendExhale(e,message,customSourceLocation.getOrElse(sourceLocation),takeComment())
      case None => // don't append exhale
    }
  }
}
