package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.programs.symbols.ProgramVariable
import silAST.methods.MethodFactory
import silAST.methods.implementations.{ImplementationFactory, BasicBlockFactory}
import util._
import silAST.expressions.{PermissionExpression, Expression, ExpressionFactory}
import silAST.source.SourceLocation
import silAST.expressions.terms.{FieldLocation, Term}
import silAST.types.permissionLE
import silAST.expressions.util.TermSequence

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

  protected def pureLanguageConstruct[T](sourceLocation : SourceLocation)(action : PureLanguageConstruct => T) =
    action(new PureLanguageConstruct(this,sourceLocation))

  protected def removeSideEffects(expr : Expression) : Expression = {
    val remover = new ExpressionTransplantation(this) {
      def translateProgramVariable(variable : ProgramVariable) =
        currentExpressionFactory.makeProgramVariableTerm(variable, variable.sourceLocation)

      override def transplant(expression : Expression) = expression match {
        case PermissionExpression(FieldLocation(ref,field),amount) =>
          // `amount ≤ perm(ref,field)`
          currentExpressionFactory.makeDomainPredicateExpression(
            permissionLE,TermSequence(amount,
              currentExpressionFactory.makePermTerm(ref,field)(expression)
            ),expression)
        case _ => super.transplant(expression)
      }
    }
    remover.transplant(expr)
  }
}