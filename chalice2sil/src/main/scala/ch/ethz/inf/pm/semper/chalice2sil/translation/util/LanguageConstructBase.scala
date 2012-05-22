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
    es.flatten.reduceOption(currentExpressionFactory.makeBinaryExpression(And()(sourceLocation), _, _)(sourceLocation)) match {
      case Some(e) => e
      case None => TrueExpression()(sourceLocation)
    }
  }

  final def fullPermission = currentExpressionFactory.makeFullPermission()(sourceLocation)

  final def noPermission = currentExpressionFactory.makeNoPermission()(sourceLocation)

  final def acc(term : Term, field : Field, permission : Term) = currentExpressionFactory.makePermissionExpression(term, field, permission)(sourceLocation)

  final implicit def domainPredicateOps(predicate : DomainPredicate) = new {
    def apply(terms : Term*) : DomainPredicateExpression =
      currentExpressionFactory.makeDomainPredicateExpression(predicate, TermSequence(terms : _*))(sourceLocation)

    def p(terms : PTerm*) : PDomainPredicateExpression = {
      currentExpressionFactory.makePDomainPredicateExpression(predicate, PTermSequence(terms : _*))(sourceLocation)
    }

    def g(terms : GTerm*) : GDomainPredicateExpression = {
      currentExpressionFactory.makeGDomainPredicateExpression(predicate, GTermSequence(terms : _*))(sourceLocation)
    }
  }

  final implicit def domainFunctionOps(function : DomainFunction) = new {
    def apply(terms : Term*) : DomainFunctionApplicationTerm = {
      if(terms.forall(_.isInstanceOf[GTerm]))
        g(terms.map(_.asInstanceOf[GTerm]) : _*)
      if(terms.forall(_.isInstanceOf[PTerm]))
        p(terms.map(_.asInstanceOf[PTerm]) : _*)
      else
        currentExpressionFactory.makeDomainFunctionApplicationTerm(function, TermSequence(terms : _*))(sourceLocation)
    }

    def p(terms : PTerm*) : PDomainFunctionApplicationTerm = {
      currentExpressionFactory.makePDomainFunctionApplicationTerm(function, PTermSequence(terms : _*))(sourceLocation)
    }

    def g(terms : GTerm*) : GDomainFunctionApplicationTerm = {
      currentExpressionFactory.makeGDomainFunctionApplicationTerm(function, GTermSequence(terms : _*))(sourceLocation)
    }
  }

  final def perm(reference : Term, field : Field) = currentExpressionFactory.makePermTerm(reference, field)(sourceLocation)

  final implicit def binaryConnectiveOps(c : BinaryConnective) = new {
    def t(lhs : Expression, rhs : Expression) = currentExpressionFactory.makeBinaryExpression(c, lhs, rhs)(sourceLocation)

    def p(lhs : PExpression, rhs : PExpression) = currentExpressionFactory.makePBinaryExpression(c, lhs, rhs)(sourceLocation)
  }

  final implicit def unaryConnectiveOps(c : UnaryConnective) = new {
    def t(o : Expression) = currentExpressionFactory.makeUnaryExpression(c, o)(sourceLocation)

    def p(o : PExpression) = currentExpressionFactory.makePUnaryExpression(c, o)(sourceLocation)
  }

  protected class PurePTermOps(term : PTerm) {
    def !(p : PredicateFactory) : PredicateExpression = currentExpressionFactory.makePPredicateExpression(term, p.predicate)(sourceLocation)

    def !(p : PredicateTranslator) : PredicateExpression = currentExpressionFactory.makePPredicateExpression(term, p.predicateFactory.predicate)(sourceLocation)

    def !(f : Field) : PFieldReadTerm = currentExpressionFactory.makePFieldReadTerm(term, f)(sourceLocation)

    def !(f : FieldTranslator) : PFieldReadTerm = this.!(f.field)

    def ===(other:PTerm) : PEqualityExpression = currentExpressionFactory.makePEqualityExpression(term,other)(sourceLocation)
  }

  final implicit def programVariableToTerm(variable : ProgramVariable) : ProgramVariableTerm = {
    assert(variable != null)
    currentExpressionFactory.makeProgramVariableTerm(variable)(sourceLocation)
  }

  implicit def termOps(term : Term) = new {
    def !(p : PredicateFactory) : PredicateExpression = currentExpressionFactory.makePredicateExpression(term, p)(sourceLocation)

    def !(p : PredicateTranslator) : PredicateExpression = currentExpressionFactory.makePredicateExpression(term, p)(sourceLocation)

    def !(f : Field) : FieldReadTerm = term match {
      case pt : PTerm => currentExpressionFactory.makePFieldReadTerm(pt, f)(sourceLocation)
      case _ => currentExpressionFactory.makeFieldReadTerm(term, f)(sourceLocation)
    }

    def !(f : FieldTranslator) : FieldReadTerm = this.!(f.field)

    def ===(other:Term) : EqualityExpression = currentExpressionFactory.makeEqualityExpression(term,other)(sourceLocation)
  }
}