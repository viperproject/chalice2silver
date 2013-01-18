package semper.chalice2sil.translation

import semper.sil.ast.programs.symbols.ProgramVariable
import util._
import semper.sil.ast.expressions.{PermissionExpression, Expression, ExpressionFactory}
import semper.sil.ast.source.SourceLocation
import semper.sil.ast.expressions.terms.FieldLocation
import semper.sil.ast.types.permissionLE
import semper.sil.ast.expressions.util.ExpressionSequence

/**
 *
 */
trait MemberEnvironment extends ProgramEnvironment {
  def programVariables: DerivedFactoryCache[chalice.Variable, String, ProgramVariable]

  def thisVariable: ProgramVariable

  def environmentReadFractionExpression(sourceLocation: SourceLocation): Expression

  def environmentCurrentThreadExpression(sourceLocation: SourceLocation): Expression

  def currentExpressionFactory: ExpressionFactory

  def nameSequence: NameSequence

  //Optional
  def getNextName(prefix: String = "") = prefix match {
    case null | "" => nameSequence.nextName
    case p => p + "_" + nameSequence.nextName
  }

  protected def pureLanguageConstruct[T](sourceLocation: SourceLocation)(action: PureLanguageConstruct => T) =
    action(new PureLanguageConstruct(this, sourceLocation))

  protected def removeSideEffects(expr: Expression): Expression = {
    val remover = new ExpressionTransplantation(this) {
      def translateProgramVariable(variable: ProgramVariable) =
        currentExpressionFactory.makeProgramVariableExpression(variable, variable.sourceLocation)

      override def transplant(expression: Expression) = expression match {
        case PermissionExpression(FieldLocation(ref, field), amount) =>
          // `amount ≤ perm(ref,field)`
          currentExpressionFactory.makeDomainPredicateExpression(
            permissionLE, ExpressionSequence(amount,
              currentExpressionFactory.makePermExpression(ref, field)(expression)
            ), expression)
        case _ => super.transplant(expression)
      }
    }
    remover.transplant(expr)
  }
}