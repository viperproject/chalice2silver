package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.types.DataTypeSequence._
import silAST.expressions.util.DTermSequence._
import silAST.symbols.logical.Not._
import silAST.expressions.util.DTermSequence
import silAST.symbols.logical.Not
import silAST.source.{noLocation}
import silAST.expressions.terms.DTerm
import silAST.domains.{DomainPredicate, DomainFunction}
import silAST.expressions.DExpression
import silAST.types.{DataType, DataTypeSequence}
import silAST.symbols.logical.quantification.{BoundVariable, Forall}

/**
 * Author: Christian Klauser
 */

class ChalicePrelude(programEnvironment : ProgramEnvironment) {
  private val names = NameSequence()
  
  private val programFactory = programEnvironment.programFactory
  private val loc = noLocation  //TODO: define a more sensible location than `noLocation` for the Prelude

  object Boolean {    
    private val factory = programFactory.getDomainFactory("Boolean",Nil)(loc)
    private def fApp(domainFunction : DomainFunction, args : DTerm*) = {
      factory.makeDDomainFunctionApplicationTerm(loc,domainFunction,DTermSequence(args:_*))
    }
    private def pApp(domainPredicate : DomainPredicate, args : DTerm*) = {
      factory.makeDDomainPredicateExpression(loc,domainPredicate,DTermSequence(args:_*))
    }
    private def not(operand : DExpression) : DExpression = 
      factory.makeDUnaryExpression(loc,silAST.symbols.logical.Not(),operand)
    private def and(lhs : DExpression, rhs : DExpression) : DExpression =
      factory.makeDBinaryExpression(loc,silAST.symbols.logical.And(),lhs,rhs)
    private def or(lhs : DExpression, rhs : DExpression) : DExpression =
      factory.makeDBinaryExpression(loc,silAST.symbols.logical.Or(),lhs,rhs)
    private def equiv(lhs : DExpression,  rhs : DExpression) : DExpression =
      factory.makeDBinaryExpression(loc,silAST.symbols.logical.Equivalence(),lhs,rhs)
    private def imply(lhs : DExpression,rhs:DExpression) : DExpression = 
      factory.makeDBinaryExpression(loc,silAST.symbols.logical.Implication(),lhs,rhs)
    private def equality(lhs : DTerm, rhs : DTerm) =
      factory.makeDEqualityExpression(loc,lhs,rhs)
    private def ∀(aType : DataType, expr : BoundVariable => DExpression) : DExpression = {
      val a = factory.makeBoundVariable(loc,names.nextName,aType)
      factory.makeDQuantifierExpression(loc,Forall,a,expr(a))
    }
    private def ∀(aType : DataType,  bType : DataType,  expr : (BoundVariable,BoundVariable) => DExpression) : DExpression = {
      val a = factory.makeBoundVariable(loc,names.nextName,aType)
      val b = factory.makeBoundVariable(loc,names.nextName,bType)
      factory.makeDQuantifierExpression(loc,Forall,a,
        factory.makeDQuantifierExpression(loc,Forall,b,expr(a,b))
      )
    }
    implicit private def boundVariableAsTerm(v : BoundVariable) : DTerm = factory.makeBoundVariableTerm(loc,v)

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Core
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val Type = programFactory.makeNonReferenceDataType(loc,factory,programFactory.emptyDTSequence)
    val TrueLiteral = factory.defineDomainFunction(loc,"True",programFactory.emptyDTSequence,Type)
    val FalseLiteral = factory.defineDomainFunction(loc,"False",programFactory.emptyDTSequence,Type)

    factory.addDomainAxiom(loc,"trueAndFalseNotEqual",
        not(equality(
          fApp(TrueLiteral),
          fApp(FalseLiteral)))
    )

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Evaluate
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val Evaluate = factory.defineDomainPredicate(loc,"EvalBool",DataTypeSequence(Type))

    factory.addDomainAxiom(loc,"evaluateBooleanTrue",factory.makeDDomainPredicateExpression(
      loc,Evaluate,DTermSequence(fApp(TrueLiteral))
    ))
    factory.addDomainAxiom(loc,"evaluateBooleanFalse",factory.makeDUnaryExpression(loc,silAST.symbols.logical.Not(),
      pApp (Evaluate,fApp(FalseLiteral))
    ))

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Unary Not
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val Not = factory.defineDomainFunction(loc,"¬",DataTypeSequence(Type),Type)
    factory.addDomainAxiom(loc,"notTrueIsFalse",equality(fApp(Not,fApp(TrueLiteral)),fApp(FalseLiteral)))
    factory.addDomainAxiom(loc,"notFalseIsTrue",equality(fApp(Not,fApp(FalseLiteral)),fApp(TrueLiteral)))

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Binary And
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val And = factory.defineDomainFunction(loc,"∧",DataTypeSequence(Type,Type),Type)
    // ∀ a,b : Boolean . Evaluate(And(a,b)) ↔ Evaluate(a) ∧ Evaluate(b)
    factory.addDomainAxiom(loc,"and",∀(Type,Type,(a,b) =>
      equiv(pApp(Evaluate,fApp(And,a,b)),and(pApp(Evaluate,a),pApp(Evaluate,b)))
    ))

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Binary Or
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val Or = factory.defineDomainFunction(loc,"∨",DataTypeSequence(Type,Type),Type)
    // ∀ a,b : Boolean . Evaluate(Or(a,b)) ↔ Evaluate(a) ∨ Evaluate(b)
    factory.addDomainAxiom(loc,"or",∀(Type,Type,(a,b) =>
      equiv(pApp(Evaluate,fApp(And,a,b)),or(pApp(Evaluate,a),pApp(Evaluate,b)))
    ))

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Binary Implication
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val Implication = factory.defineDomainFunction(loc,"→",DataTypeSequence(Type,Type),Type)
    // ∀ a,b : Boolean . Evaluate(Implication(a,b)) ↔ Evaluate(a) → Evaluate(b)
    factory.addDomainAxiom(loc,"implication",∀(Type,Type,(a,b) =>
      equiv(pApp(Evaluate,fApp(Implication,a,b)),imply(pApp(Evaluate,a),pApp(Evaluate,b)))
    ))

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Binary Equivalence
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val Equivalence = factory.defineDomainFunction(loc,"↔",DataTypeSequence(Type,Type),Type)
    // ∀ a,b : Boolean . Evaluate(Equivalence(a,b)) ↔ Evaluate(a) = Evaluate(b)
    factory.addDomainAxiom(loc,"equivalence",∀(Type,Type,(a,b) =>
      equiv(pApp(Evaluate,fApp(Implication,a,b)),equiv(pApp(Evaluate,a),pApp(Evaluate,b)))
    ))

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Compile Domain
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    lazy val Domain = factory.compile()
  }
}