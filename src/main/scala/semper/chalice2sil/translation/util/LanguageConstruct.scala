package semper.chalice2sil.translation.util

import scala.language.implicitConversions
import semper.chalice2sil._
import translation._
import semper.sil.ast.source.SourceLocation
import collection.TraversableOnce
import semper.sil.ast.expressions.util.ExpressionSequence
import semper.sil.ast.methods.{MethodFactory, Method}
import semper.sil.ast.types.referenceType
import semper.sil.ast.symbols.logical.And
import semper.sil.ast.programs.symbols.{PredicateFactory, ProgramVariable, Field}
import semper.sil.ast.expressions.Expression


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
    def <--(rhs : Expression) { scope.currentBlock.appendFieldAssignment(variable,field,rhs,sourceLocation,takeComment()) }
    def <--(rhs : (ProgramVariable, Field)) {
      this <-- scope.currentExpressionFactory.makeFieldReadExpression(
        scope.currentExpressionFactory.makeProgramVariableExpression(rhs._1,sourceLocation),
        rhs._2,
        sourceLocation) }
  }

  final implicit def heapLocationOps(location : (ProgramVariable,Field)) : ImpureHeapLocationOps = new ImpureHeapLocationOps(location._1,location._2)
  final implicit def heapLocationOpsT(location : (ProgramVariable, FieldTranslator)) : ImpureHeapLocationOps = new ImpureHeapLocationOps(location._1,location._2)

  final type MethodCallSpec = (Expression,MethodFactory,Seq[Expression])

  final implicit def programVariableSeqOps(vars : Seq[ProgramVariable]) = new {
    def <--(call : MethodCallSpec) {
      scope.currentBlock.appendCall(
        currentExpressionFactory.makeProgramVariableSequence(vars,sourceLocation),
        call._1,
        call._2,
        ExpressionSequence(call._3:_*),sourceLocation,takeComment())
    }
    def <--(rhss : Iterable[Expression]) {
      vars.zip(rhss).foreach(t => scope.currentBlock.appendAssignment(t._1,t._2,sourceLocation,takeComment()))
    }
  }

  protected class ImpureProgramVariableOps protected[LanguageConstruct](variable : ProgramVariable)
    extends PureProgramVariableOps(variable) {
    def <--(rhs : Expression) { scope.currentBlock.appendAssignment(variable,rhs,sourceLocation,takeComment()) }
    def <--(call : MethodCallSpec) { Seq(variable) <-- call }
    def <--(newReferenceTag : NewRef) { scope.currentBlock.appendNew(variable,referenceType,sourceLocation,takeComment()) }
  }

  final implicit def programVariableOps(variable : ProgramVariable) : ImpureProgramVariableOps = new ImpureProgramVariableOps(variable)
  
  final case class NewRef()

  class ImpureExpressionOps(term : Expression) extends PureExpressionOps(term) {
    def !(mc : (Method,TraversableOnce[Expression])) = (term,mc._1,mc._2)
  }
  
  final implicit def pExpressionOps(term : Expression) : ImpureExpressionOps = new ImpureExpressionOps(term)
  
  class MethodOps protected[LanguageConstruct](method : MethodFactory) {
    def apply(args : TraversableOnce[Expression]) = (method,args) 
  }
  
  final implicit def methodOps(method : MethodFactory) = new MethodOps(method)
  final implicit def methodOpsT(method : MethodTranslator) = new MethodOps(method.methodFactory)

  final def fold(receiver : Expression, predicate : PredicateFactory,permission : Expression) {
    scope.currentBlock.appendFold(receiver,predicate,permission,sourceLocation,takeComment())
  }

  final def unfold(receiver : Expression, predicate : PredicateFactory,permission : Expression) {
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
