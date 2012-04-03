package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import silAST.source.SourceLocation
import silAST.expressions.util.{GTermSequence, PTermSequence, TermSequence}
import silAST.expressions._
import silAST.domains.{DomainFunction, DomainPredicate}
import terms._
import silAST.symbols.logical.{UnaryConnective, BinaryConnective, And}
import silAST.programs.symbols.{PredicateFactory, ProgramVariable, Field}
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

  final def conjunction(es : TraversableOnce[Expression]*) : Expression = {
    es.flatten.reduceOption(currentExpressionFactory.makeBinaryExpression(sourceLocation, And()(sourceLocation), _, _)) match {
      case Some(e) => e
      case None => TrueExpression()(sourceLocation)
    }
  }

  final def fullPermission = currentExpressionFactory.makeFullPermission(sourceLocation)

  final def noPermission = currentExpressionFactory.makeNoPermission(sourceLocation)

  final def acc(term : Term, field : Field, permission : Term) = currentExpressionFactory.makePermissionExpression(sourceLocation, term, field, permission)

  final implicit def domainPredicateOps(predicate : DomainPredicate) = new {
    def apply(terms : Term*) : DomainPredicateExpression =
      currentExpressionFactory.makeDomainPredicateExpression(sourceLocation, predicate, TermSequence(terms : _*))

    def p(terms : PTerm*) : PDomainPredicateExpression = {
      currentExpressionFactory.makePDomainPredicateExpression(sourceLocation, predicate, PTermSequence(terms : _*))
    }

    def g(terms : GTerm*) : GDomainPredicateExpression = {
      currentExpressionFactory.makeGDomainPredicateExpression(sourceLocation, predicate, GTermSequence(terms : _*))
    }
  }

  final implicit def domainFunctionOps(function : DomainFunction) = new {
    def apply(terms : Term*) : DomainFunctionApplicationTerm = {
      if(terms.forall(_.isInstanceOf[GTerm]))
        g(terms.map(_.asInstanceOf[GTerm]) : _*)
      if(terms.forall(_.isInstanceOf[PTerm]))
        p(terms.map(_.asInstanceOf[PTerm]) : _*)
      else
        currentExpressionFactory.makeDomainFunctionApplicationTerm(sourceLocation, function, TermSequence(terms : _*))
    }

    def p(terms : PTerm*) : PDomainFunctionApplicationTerm = {
      currentExpressionFactory.makePDomainFunctionApplicationTerm(sourceLocation, function, PTermSequence(terms : _*))
    }

    def g(terms : GTerm*) : GDomainFunctionApplicationTerm = {
      currentExpressionFactory.makeGDomainFunctionApplicationTerm(sourceLocation, function, GTermSequence(terms : _*))
    }
  }

  final def perm(reference : Term, field : Field) = currentExpressionFactory.makePermTerm(sourceLocation, reference, field)

  final implicit def binaryConnectiveOps(c : BinaryConnective) = new {
    def t(lhs : Expression, rhs : Expression) = currentExpressionFactory.makeBinaryExpression(sourceLocation, c, lhs, rhs)

    def p(lhs : PExpression, rhs : PExpression) = currentExpressionFactory.makePBinaryExpression(sourceLocation, c, lhs, rhs)
  }

  final implicit def unaryConnectiveOps(c : UnaryConnective) = new {
    def t(o : Expression) = currentExpressionFactory.makeUnaryExpression(sourceLocation, c, o)

    def p(o : PExpression) = currentExpressionFactory.makePUnaryExpression(sourceLocation, c, o)
  }

  protected class PurePTermOps(term : PTerm) {
    def !(p : PredicateFactory) : PredicateExpression = currentExpressionFactory.makePPredicateExpression(sourceLocation, term, p.predicate)

    def !(p : PredicateTranslator) : PredicateExpression = currentExpressionFactory.makePPredicateExpression(sourceLocation, term, p.predicateFactory.predicate)

    def !(f : Field) : PFieldReadTerm = currentExpressionFactory.makePFieldReadTerm(sourceLocation, term, f)

    def !(f : FieldTranslator) : PFieldReadTerm = this.!(f.field)
  }

  final implicit def programVariableToTerm(variable : ProgramVariable) : ProgramVariableTerm =
    currentExpressionFactory.makeProgramVariableTerm(sourceLocation, variable)

  implicit def termOps(term : Term) = new {
    def !(p : PredicateFactory) : PredicateExpression = currentExpressionFactory.makePredicateExpression(sourceLocation, term, p)

    def !(p : PredicateTranslator) : PredicateExpression = currentExpressionFactory.makePredicateExpression(sourceLocation, term, p)

    def !(f : Field) : FieldReadTerm = term match {
      case pt : PTerm => currentExpressionFactory.makePFieldReadTerm(sourceLocation, pt, f)
      case _ => currentExpressionFactory.makeFieldReadTerm(sourceLocation, term, f)
    }

    def !(f : FieldTranslator) : FieldReadTerm = this.!(f.field)
  }
}
