package semper.chalice2sil.translation.util

import semper.sil.ast.source.SourceLocation
import semper.sil.ast.expressions.util.ExpressionSequence
import semper.sil.ast.expressions._
import semper.sil.ast.domains.{DomainFunction, DomainPredicate}
import terms._
import semper.sil.ast.symbols.logical.{Not, UnaryConnective, BinaryConnective, And}
import semper.sil.ast.programs.symbols.{PredicateFactory, ProgramVariable, Field}
import semper.chalice2sil._
import translation.{FieldTranslator, MemberEnvironment}

class LanguageConstructBase(val environment : MemberEnvironment, val sourceLocation : SourceLocation) {

  import environment._

  protected class PureHeapLocationOps protected[util](val variable : ProgramVariable, val field : Field) {
  }

  protected class PureProgramVariableOps protected[util](variable : ProgramVariable) {
    def !(f : Field) = (variable, f)

    def !(ft : FieldTranslator) = (variable, ft)
  }

  final def conjunction(es : Expression*) : Expression = {
    es.reduceOption(currentExpressionFactory.makeBinaryExpression(And()(sourceLocation), _, _, sourceLocation)) match {
      case Some(e) => e
      case None => TrueExpression()(sourceLocation)
    }
  }

  final def fullPermission = currentExpressionFactory.makeFullPermission(sourceLocation)

  final def noPermission = currentExpressionFactory.makeNoPermission(sourceLocation)

  final def acc(term : Expression, field : Field, permission : Expression) = currentExpressionFactory.makeFieldPermissionExpression(term, field, permission, sourceLocation)
  final def acc(term : Expression, predicate : PredicateFactory, permission : Expression) = currentExpressionFactory.makePredicatePermissionExpression(term,predicate,permission, sourceLocation)

  final implicit def domainPredicateOps(predicate : DomainPredicate) = new {
    def apply(terms : Expression*) : DomainPredicateExpression =
      currentExpressionFactory.makeDomainPredicateExpression(predicate, ExpressionSequence(terms : _*), sourceLocation)

    def p(terms : Expression*) : DomainPredicateExpression = apply(terms:_*)
    def g(terms : Expression*) : DomainPredicateExpression = apply(terms:_*)
  }

  final implicit def domainFunctionOps(function : DomainFunction) = new {
    def apply(terms : Expression*) : DomainFunctionApplicationExpression = {
      currentExpressionFactory.makeDomainFunctionApplicationExpression(function, ExpressionSequence(terms : _*), sourceLocation)
    }

    def t(terms : Expression*) : DomainFunctionApplicationExpression = apply(terms:_*)
    def p(terms : Expression*) : DomainFunctionApplicationExpression = apply(terms:_*)
    def g(terms : Expression*) : DomainFunctionApplicationExpression = apply(terms:_*)
  }

  final def perm(reference : Expression, field : Field) = currentExpressionFactory.makePermExpression(reference, field)(sourceLocation)

  final implicit def binaryConnectiveOps(c : BinaryConnective) = new {
    def t(lhs : Expression, rhs : Expression) = currentExpressionFactory.makeBinaryExpression(c, lhs, rhs, sourceLocation)
    def p(lhs : Expression, rhs : Expression) = currentExpressionFactory.makeBinaryExpression(c, lhs, rhs, sourceLocation)
  }

  final implicit def unaryConnectiveOps(c : UnaryConnective) = new {
    def t(o : Expression) = currentExpressionFactory.makeUnaryExpression(c, o, sourceLocation)
    def p(o : Expression) = currentExpressionFactory.makeUnaryExpression(c, o, sourceLocation)
  }

  protected class PureExpressionOps(term : Expression) {
    def !(f : Field) : FieldReadExpression = currentExpressionFactory.makeFieldReadExpression(term, f, sourceLocation)

    def !(f : FieldTranslator) : FieldReadExpression = this.!(f.field)

    def ===(other:Expression) : EqualityExpression = currentExpressionFactory.makeEqualityExpression(term,other, sourceLocation)
    def =/=(other:Expression) : Expression = currentExpressionFactory.makeUnaryExpression(Not()(sourceLocation),
      currentExpressionFactory.makeEqualityExpression(term,other,sourceLocation)
      ,sourceLocation)
  }

  final implicit def programVariableToExpression(variable : ProgramVariable) : ProgramVariableExpression = {
    assert(variable != null)
    currentExpressionFactory.makeProgramVariableExpression(variable, sourceLocation)
  }

  implicit def termOps(term : Expression) = new {
    def !(f : Field) : FieldReadExpression = term match {
      case pt : Expression => currentExpressionFactory.makeFieldReadExpression(pt, f, sourceLocation)
      case _ => currentExpressionFactory.makeFieldReadExpression(term, f, sourceLocation)
    }

    def !(f : FieldTranslator) : FieldReadExpression = this.!(f.field)

    def ===(other:Expression) : EqualityExpression = currentExpressionFactory.makeEqualityExpression(term,other,sourceLocation)
    def =/=(other:Expression) : Expression = currentExpressionFactory.makeUnaryExpression(Not()(sourceLocation),
      currentExpressionFactory.makeEqualityExpression(term,other,sourceLocation)
      ,sourceLocation)
  }

  implicit def intToLiteral(integer : Int) : Expression = currentExpressionFactory.makeIntegerLiteralExpression(integer,sourceLocation)
}
