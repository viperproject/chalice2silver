package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import semper.sil.ast.source.SourceLocation
import semper.sil.ast.expressions.util.{GTermSequence, PTermSequence, TermSequence}
import semper.sil.ast.expressions._
import semper.sil.ast.domains.{DomainFunction, DomainPredicate}
import terms._
import semper.sil.ast.symbols.logical.{Not, UnaryConnective, BinaryConnective, And}
import semper.sil.ast.programs.symbols.{PredicateFactory, ProgramVariable, Field}
import ch.ethz.inf.pm.semper.chalice2sil._
import translation.{PredicateTranslator, FieldTranslator, MemberEnvironment}

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

  final def acc(term : Term, field : Field, permission : Term) = currentExpressionFactory.makeFieldPermissionExpression(term, field, permission, sourceLocation)
  final def acc(term : Term, predicate : PredicateFactory, permission : Term) = currentExpressionFactory.makePredicatePermissionExpression(term,predicate,permission, sourceLocation)

  final implicit def domainPredicateOps(predicate : DomainPredicate) = new {
    def apply(terms : Term*) : DomainPredicateExpression =
      currentExpressionFactory.makeDomainPredicateExpression(predicate, TermSequence(terms : _*), sourceLocation)

    def p(terms : PTerm*) : PDomainPredicateExpression = {
      currentExpressionFactory.makePDomainPredicateExpression(predicate, PTermSequence(terms : _*), sourceLocation)
    }

    def g(terms : GTerm*) : GDomainPredicateExpression = {
      currentExpressionFactory.makeGDomainPredicateExpression(predicate, GTermSequence(terms : _*), sourceLocation)
    }
  }

  final implicit def domainFunctionOps(function : DomainFunction) = new {
    def apply(terms : Term*) : DomainFunctionApplicationTerm = {
      if(terms.forall(_.isInstanceOf[GTerm]))
        g(terms.map(_.asInstanceOf[GTerm]) : _*)
      if(terms.forall(_.isInstanceOf[PTerm]))
        p(terms.map(_.asInstanceOf[PTerm]) : _*)
      else
        currentExpressionFactory.makeDomainFunctionApplicationTerm(function, TermSequence(terms : _*), sourceLocation)
    }

    def p(terms : PTerm*) : PDomainFunctionApplicationTerm = {
      currentExpressionFactory.makePDomainFunctionApplicationTerm(function, PTermSequence(terms : _*), sourceLocation)
    }

    def g(terms : GTerm*) : GDomainFunctionApplicationTerm = {
      currentExpressionFactory.makeGDomainFunctionApplicationTerm(function, GTermSequence(terms : _*), sourceLocation)
    }

    def t(terms : Term*) : DomainFunctionApplicationTerm = apply(terms:_*)
  }

  final def perm(reference : Term, field : Field) = currentExpressionFactory.makePermTerm(reference, field)(sourceLocation)

  final implicit def binaryConnectiveOps(c : BinaryConnective) = new {
    def t(lhs : Expression, rhs : Expression) = currentExpressionFactory.makeBinaryExpression(c, lhs, rhs, sourceLocation)

    def p(lhs : PExpression, rhs : PExpression) = currentExpressionFactory.makePBinaryExpression(c, lhs, rhs, sourceLocation)
  }

  final implicit def unaryConnectiveOps(c : UnaryConnective) = new {
    def t(o : Expression) = currentExpressionFactory.makeUnaryExpression(c, o, sourceLocation)

    def p(o : PExpression) = currentExpressionFactory.makePUnaryExpression(c, o, sourceLocation)
  }

  protected class PurePTermOps(term : PTerm) {
    def !(f : Field) : PFieldReadTerm = currentExpressionFactory.makePFieldReadTerm(term, f, sourceLocation)

    def !(f : FieldTranslator) : PFieldReadTerm = this.!(f.field)

    def ===(other:PTerm) : PEqualityExpression = currentExpressionFactory.makePEqualityExpression(term,other, sourceLocation)
    def =/=(other:PTerm) : PExpression = currentExpressionFactory.makePUnaryExpression(Not()(sourceLocation),
      currentExpressionFactory.makePEqualityExpression(term,other,sourceLocation)
      ,sourceLocation)
  }

  final implicit def programVariableToTerm(variable : ProgramVariable) : ProgramVariableTerm = {
    assert(variable != null)
    currentExpressionFactory.makeProgramVariableTerm(variable, sourceLocation)
  }

  implicit def termOps(term : Term) = new {
    def !(f : Field) : FieldReadTerm = term match {
      case pt : PTerm => currentExpressionFactory.makePFieldReadTerm(pt, f, sourceLocation)
      case _ => currentExpressionFactory.makeFieldReadTerm(term, f, sourceLocation)
    }

    def !(f : FieldTranslator) : FieldReadTerm = this.!(f.field)

    def ===(other:Term) : EqualityExpression = currentExpressionFactory.makeEqualityExpression(term,other,sourceLocation)
    def =/=(other:Term) : Expression = currentExpressionFactory.makeUnaryExpression(Not()(sourceLocation),
      currentExpressionFactory.makeEqualityExpression(term,other,sourceLocation)
      ,sourceLocation)
  }

  implicit def intToLiteral(integer : Int) : Term = currentExpressionFactory.makeIntegerLiteralTerm(integer,sourceLocation)
}
