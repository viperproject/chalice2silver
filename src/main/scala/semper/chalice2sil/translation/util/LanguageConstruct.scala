package semper.chalice2sil.translation.util

import semper.chalice2sil._
import translation._
import semper.sil.ast.source.SourceLocation
import collection.TraversableOnce
import semper.sil.ast.expressions.util.TermSequence
import semper.sil.ast.methods.{MethodFactory, Method}

import semper.sil.ast.types.referenceType
import semper.sil.ast.expressions.Expression
import semper.sil.ast.symbols.logical.And
import semper.sil.ast.programs.symbols.{PredicateFactory, ProgramVariable, Field}
import semper.sil.ast.expressions.terms.Term


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
    def <--(rhs : Term) { scope.currentBlock.appendFieldAssignment(variable,field,rhs,sourceLocation,takeComment()) }
    def <--(rhs : (ProgramVariable, Field)) {
      this <-- scope.currentExpressionFactory.makeFieldReadTerm(
        scope.currentExpressionFactory.makeProgramVariableTerm(rhs._1,sourceLocation),
        rhs._2,
        sourceLocation) }
  }

  final implicit def heapLocationOps(location : (ProgramVariable,Field)) : ImpureHeapLocationOps = new ImpureHeapLocationOps(location._1,location._2)
  final implicit def heapLocationOpsT(location : (ProgramVariable, FieldTranslator)) : ImpureHeapLocationOps = new ImpureHeapLocationOps(location._1,location._2)

  final type MethodCallSpec = (Term,MethodFactory,Seq[Term])

  final implicit def programVariableSeqOps(vars : Seq[ProgramVariable]) = new {
    def <--(call : MethodCallSpec) {
      scope.currentBlock.appendCall(
        currentExpressionFactory.makeProgramVariableSequence(vars,sourceLocation),
        call._1,
        call._2,
        TermSequence(call._3:_*),sourceLocation,takeComment())
    }
    def <--(rhss : Iterable[Term]) {
      vars.zip(rhss).foreach(t => scope.currentBlock.appendAssignment(t._1,t._2,sourceLocation,takeComment()))
    }
  }

  protected class ImpureProgramVariableOps protected[LanguageConstruct](variable : ProgramVariable)
    extends PureProgramVariableOps(variable) {
    def <--(rhs : Term) { scope.currentBlock.appendAssignment(variable,rhs,sourceLocation,takeComment()) }
    def <--(call : MethodCallSpec) { Seq(variable) <-- call }
    def <--(newReferenceTag : NewRef) { scope.currentBlock.appendNew(variable,referenceType,sourceLocation,takeComment()) }
  }

  final implicit def programVariableOps(variable : ProgramVariable) : ImpureProgramVariableOps = new ImpureProgramVariableOps(variable)
  
  final case class NewRef()

  class ImpureTermOps(term : Term) extends PureTermOps(term) {
    def !(mc : (Method,TraversableOnce[Term])) = (term,mc._1,mc._2)
  }
  
  final implicit def pTermOps(term : Term) : ImpureTermOps = new ImpureTermOps(term)
  
  class MethodOps protected[LanguageConstruct](method : MethodFactory) {
    def apply(args : TraversableOnce[Term]) = (method,args) 
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
