package ch.ethz.inf.pm.semper.chalice2sil.translation

import operators.Lookup.{Failure, Ambiguous, Success}
import silAST.expressions.terms.Term
import silAST.expressions.util.TermSequence._
import silAST.source.SourceLocation
import math.BigInt._
import silAST.expressions.ExpressionFactory
import silAST.expressions.util.TermSequence
import silAST.programs.symbols.ProgramVariable
import silAST.types.DataType
import ch.ethz.inf.pm.semper.chalice2sil._

/**
  * @author Christian Klauser
  */
trait TermTranslator extends MethodEnvironment with TypeTranslator {

  def translateTerm(expression : chalice.RValue) : Term = termTranslation(expression)

  protected def matchingTerm(partialFunction : PartialFunction[chalice.RValue, Term]) = partialFunction
  
  def dummyTerm(location : SourceLocation) = currentExpressionFactory.makeIntegerLiteralTerm(location,27)
  
  protected def termTranslation : PartialFunction[chalice.RValue,Term] = matchingTerm {
     //RValue is (expression ∪ new-obj)
      //NewRhs  is used for both object creation and channel creation (where lower and upper bounds come into play)
      case rvalue@chalice.NewRhs(typeId,init,lowerBound,upperBound) =>
        report(messages.UnknownAstNode(rvalue))
        dummyTerm(rvalue)
      // from here on, match against the cases of Expression
      case rvalue@chalice.IntLiteral(i) =>
        currentExpressionFactory.makeIntegerLiteralTerm(rvalue, i)
      case rvalue@chalice.BoolLiteral(true) =>
        currentExpressionFactory.makeDomainFunctionApplicationTerm(rvalue,prelude.Boolean.trueLiteral,TermSequence())
      case rvalue@chalice.BoolLiteral(false) =>
        currentExpressionFactory.makeDomainFunctionApplicationTerm(rvalue,prelude.Boolean.falseLiteral,TermSequence())
      case variableExpr:chalice.VariableExpr =>
        currentExpressionFactory.makeProgramVariableTerm(variableExpr,(localVariableVersion(variableExpr.v)))
      case rvalue@chalice.Old(e) => currentExpressionFactory.makeOldTerm(rvalue,translateTerm(e))
      case access@chalice.MemberAccess(rcvr,_) if !access.isPredicate =>
        assert(access.f != null,"Chalice MemberAccess node (%s) is not linked to a field.".format(access))
        currentExpressionFactory.makeFieldReadTerm(access,translateTerm(rcvr),fields(access.f))
      case th@chalice.ImplicitThisExpr() =>
        currentExpressionFactory.makeProgramVariableTerm(th,thisVariable)
      case th@chalice.ExplicitThisExpr() =>
        currentExpressionFactory.makeProgramVariableTerm(th,thisVariable)
      case rvalue@chalice.And(lhs,rhs) =>
        currentExpressionFactory.makeDomainFunctionApplicationTerm(rvalue,prelude.Boolean.logicalAnd,TermSequence(
          translateTerm(lhs),
          translateTerm(rhs)
        ))
      case rvalue@chalice.Or(lhs,rhs) =>
        currentExpressionFactory.makeDomainFunctionApplicationTerm(rvalue,prelude.Boolean.logicalOr,TermSequence(
          translateTerm(lhs),
          translateTerm(rhs)
        ))
      case rvalue@chalice.Implies(lhs,rhs) =>
        currentExpressionFactory.makeDomainFunctionApplicationTerm(rvalue,prelude.Boolean.implication,TermSequence(
          translateTerm(lhs),
          translateTerm(rhs)
        ))
      case rvalue@chalice.Not(op) =>
        currentExpressionFactory.makeDomainFunctionApplicationTerm(rvalue,prelude.Boolean.not,TermSequence(
          translateTerm(op)
        ))
      case binary:chalice.BinaryExpr => translateBinaryExpression(binary)
  }

  protected def translateBinaryExpression(binary : chalice.BinaryExpr) : Term = {
    val (lhs,rhs) = (binary.E0,binary.E1)
    // binary.ExpectedXhsType is often null, use the "inferred" types for the operands instead
    val (lhsType,rhsType,resultType) = (translateClassRef(lhs.typ),translateClassRef(rhs.typ),translateClassRef(binary.ResultType))

    domainFunctionLookup.lookup(List(lhsType,rhsType,resultType).map(_.domain))(binary.OpName,List(Some(lhsType),Some(rhsType))) match {
      case Success(e) => currentExpressionFactory.makeDomainFunctionApplicationTerm(binary,e,TermSequence(translateTerm(lhs),translateTerm(rhs)))
      case _ =>
        report(messages.OperatorNotFound(binary,lhsType,rhsType,resultType))
        dummyTerm(binary)
    }
  }

}
