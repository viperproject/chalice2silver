package semper.chalice2sil.translation

import semper.sil.ast.programs.symbols.ProgramVariable
import semper.sil.ast.methods.MethodFactory
import semper.sil.ast.methods.implementations.{ImplementationFactory, BasicBlockFactory}
import util._
import semper.sil.ast.expressions.{PermissionExpression, Expression, ExpressionFactory}
import semper.sil.ast.source.SourceLocation
import semper.sil.ast.expressions.terms.{FieldLocation, Term}
import semper.sil.ast.types.permissionLE
import semper.sil.ast.expressions.util.TermSequence

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