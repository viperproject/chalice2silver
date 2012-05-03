package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.types.DataTypeSequence._
import silAST.expressions.util.DTermSequence._
import silAST.symbols.logical.Not._
import silAST.expressions.util.DTermSequence
import silAST.symbols.logical.Not
import silAST.expressions.terms.DTerm
import silAST.expressions.DExpression
import silAST.source.{SourceLocation, noLocation}
import silAST.domains.{Domain, DomainPredicate, DomainFunction}
import silAST.types._
import silAST.symbols.logical.quantification.{LogicalVariable, Forall}
import util.{FactoryHashCache, NameSequence}

/**
 * Author: Christian Klauser
 */

class ChalicePrelude(programEnvironment : ProgramEnvironment) { prelude =>
  private val names = NameSequence()
  
  private val loc = new SourceLocation{
    override def toString = "Chalice#built-in"
  }  //TODO: define a more sensible location than `noLocation` for the Prelude

  protected class DomainEnvironment(domainName: String, typeVariableNames: Seq[(SourceLocation,String)] = Nil) extends DerivedProgramEnvironment(programEnvironment){
    val factory = programFactory.getDomainFactory(domainName, typeVariableNames)(loc)

    protected def fApp(domainFunction : DomainFunction, args : DTerm*) = {
      factory.makeDDomainFunctionApplicationTerm(domainFunction,DTermSequence(args:_*))(loc)
    }
    protected def pApp(domainPredicate : DomainPredicate, args : DTerm*) = {
      factory.makeDDomainPredicateExpression(domainPredicate,DTermSequence(args:_*))(loc)
    }
    protected def not(operand : DExpression) : DExpression =
      factory.makeDUnaryExpression(silAST.symbols.logical.Not()(loc),operand)(loc)
    protected def ∀(aType : DataType, expr : LogicalVariable => DExpression) : DExpression = {
      val a = factory.makeBoundVariable(names.nextName,aType)(loc)
      factory.makeDQuantifierExpression(Forall()(loc),a,expr(a))(loc)
    }
    protected def ∀(aType : DataType,  bType : DataType,  expr : (LogicalVariable,LogicalVariable) => DExpression) : DExpression = {
      val a = factory.makeBoundVariable(names.nextName,aType)(loc)
      val b = factory.makeBoundVariable(names.nextName,bType)(loc)
      factory.makeDQuantifierExpression(Forall()(loc),a,
        factory.makeDQuantifierExpression(Forall()(loc),b,expr(a,b))(loc)
      )(loc)
    }
    protected def ∀(aType : DataType,  bType : DataType, cType : DataType,  expr : (LogicalVariable,LogicalVariable, LogicalVariable) => DExpression) : DExpression = {
      val a = factory.makeBoundVariable(names.nextName,aType)(loc)
      val b = factory.makeBoundVariable(names.nextName,bType)(loc)
      val c = factory.makeBoundVariable(names.nextName,cType)(loc)
      factory.makeDQuantifierExpression(Forall()(loc),a,
        factory.makeDQuantifierExpression(Forall()(loc),b,
          factory.makeDQuantifierExpression(Forall()(loc),c,expr(a,b,c))(loc))(loc))(loc)
    }
    protected def ∀(
                     aType : DataType,  
                     bType : DataType, 
                     cType : DataType, 
                     dType : DataType,  
                     expr : (LogicalVariable,LogicalVariable, LogicalVariable,LogicalVariable) => DExpression) : DExpression = {
      val a = factory.makeBoundVariable(names.nextName,aType)(loc)
      val b = factory.makeBoundVariable(names.nextName,bType)(loc)
      val c = factory.makeBoundVariable(names.nextName,cType)(loc)
      val d = factory.makeBoundVariable(names.nextName,dType)(loc)
      factory.makeDQuantifierExpression(Forall()(loc),a,
        factory.makeDQuantifierExpression(Forall()(loc),b,
          factory.makeDQuantifierExpression(Forall()(loc),c,
            factory.makeDQuantifierExpression(Forall()(loc),d,expr(a,b,c,d))(loc))(loc))(loc))(loc)
    }
    implicit protected def boundVariableAsTerm(v : LogicalVariable) : DTerm = factory.makeBoundVariableTerm(v)(loc)
    implicit protected def directlyApplyDomainFunction(df : DomainFunction) = new {
      def apply(args : DTerm*) = fApp(df,args:_*)
    }
    implicit protected def directlyApplyDomainPredicate(dp : DomainPredicate) = new {
      def apply(args : DTerm*) = pApp(dp,args:_*)
    }

    implicit protected def termOps(lhs : DTerm) = new {
      def ≡(rhs : DTerm) =
        factory.makeDEqualityExpression(lhs,rhs)(loc)
      def ≠(rhs : DTerm) = not(≡(rhs))
    }
    implicit protected def logicalVariableOps(v : LogicalVariable) = new {
      protected def lhs : DTerm = v
      def ≡(rhs : DTerm) =
        factory.makeDEqualityExpression(lhs,rhs)(loc)
      def ≠(rhs : DTerm) = not(lhs ≡ rhs)
    }
    implicit protected def exprOps(lhs : DExpression) = new {
      // don't use ∧ or ∨ for and/or to take advantage of Scala operator precedence rules.
      def and(rhs : DExpression) : DExpression =
        factory.makeDBinaryExpression(silAST.symbols.logical.And()(loc),lhs,rhs)(loc)
      def or(rhs : DExpression) : DExpression =
        factory.makeDBinaryExpression(silAST.symbols.logical.Or()(loc),lhs,rhs)(loc)
      def ↔(rhs : DExpression) : DExpression =
        factory.makeDBinaryExpression(silAST.symbols.logical.Equivalence()(loc),lhs,rhs)(loc)
      def →(rhs:DExpression) : DExpression =
        factory.makeDBinaryExpression(silAST.symbols.logical.Implication()(loc),lhs,rhs)(loc)
    }
  } 
  
  object Boolean {

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Core
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val dataType = booleanType
    val trueLiteral = booleanTrue
    val falseLiteral = booleanFalse

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Evaluate
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val eval = booleanEvaluate

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Unary Not
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val not = booleanNegation

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Binary And
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val logicalAnd = booleanConjunction

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Binary Or
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val logicalOr = booleanDisjunction

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Binary Implication
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val implication = booleanImplication

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Binary Equivalence
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val equivalence = booleanEquivalence

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Compile Domain
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val domain = booleanDomain
  }

  object Token {
    val dataType = referenceType
    lazy val joinable : FieldTranslator = {
      val f = programEnvironment.programFactory.defineField("joinable",Boolean.dataType)(loc)
      val ft = new FieldTranslator(f,programEnvironment.fields.getNextId,programEnvironment)
      programEnvironment.fields.addExternal(ft)
      ft
    }
  }
  
  object Predicate extends DomainEnvironment("Predicate",Seq()) {
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Read Fraction
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val globalReadFraction = factory.defineDomainFunction("globalReadFraction",DataTypeSequence(),permissionType)(loc)

    val readFraction = factory.defineDomainFunction("readFraction",DataTypeSequence(integerType, referenceType),permissionType)(loc)
    factory.addDomainAxiom("globalReadFraction",
      ∀(integerType,referenceType, (pred,ref) => readFraction(pred,ref) ≡ globalReadFraction())
    )(loc)
  }

  object Monitor extends DomainEnvironment("Monitor",Seq()) {
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Read Fraction
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val globalReadFraction = factory.defineDomainFunction("globalReadFraction",DataTypeSequence(),permissionType)(loc)
    factory.addDomainAxiom("monitors_and_predicates_are_the_same",
      Predicate.globalReadFraction() ≡ Monitor.globalReadFraction())(loc)

    val readFraction = factory.defineDomainFunction("readFraction",DataTypeSequence(referenceType),permissionType)(loc)
    factory.addDomainAxiom("globalReadFraction",
      ∀(referenceType, (ref) => readFraction(ref) ≡ globalReadFraction())
    )(loc)
  }

  object Pair {
    object Template extends DomainEnvironment("Pair",Seq((loc,"A"),(loc,"B"))){
      val firstType = factory.makeVariableType(factory.typeVariables.find(_.name ==  "A").get)(loc)
      var secondType = factory.makeVariableType(factory.typeVariables.find(_.name == "B").get)(loc)
      val dataType = factory.makeNonReferenceDataType(factory,DataTypeSequence(firstType,secondType))(loc)

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Constructors
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val create = factory.defineDomainFunction("create",DataTypeSequence(firstType,secondType),dataType)(loc)

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Operations
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val getFirst =  factory.defineDomainFunction("getFirst",DataTypeSequence(dataType),firstType)(loc)
      factory.addDomainAxiom("getFirst",
        ∀(firstType,secondType,(a,b) => getFirst(create(a,b)) ≡ a))(loc)

      val getSecond = factory.defineDomainFunction("getSecond",DataTypeSequence(dataType),secondType)(loc)
      factory.addDomainAxiom("getSecond",
        ∀(firstType,secondType,(a,b) => getSecond(create(a,b)) ≡ b))(loc)

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Axioms
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      factory.addDomainAxiom("equality",∀(firstType,secondType,firstType,secondType,
        (a,b,x,y) => (
          (create(a,b) ≡ create(x,y)) ↔ (a ≡ x and (b ≡ y))
        )))(loc)
    }
    case class PreludeDomainInfo protected[Pair] (firstType : DataType, secondType : DataType) {
      val domain = programEnvironment.programFactory.makeDomainInstance(Template.factory,DataTypeSequence(firstType,secondType))
      val dataType = programEnvironment.programFactory.makeNonReferenceDataType(Template.factory,DataTypeSequence(firstType,secondType))(loc)

      val create = domain.functions.find(_.name == "create").get
      val getFirst = domain.functions.find(_.name == "getFirst").get
      val getSecond = domain.functions.find(_.name == "getSecond").get
    }
    val instances = new FactoryHashCache[(DataType,DataType),PreludeDomainInfo] {
      protected def construct(t : (DataType, DataType)) = PreludeDomainInfo(t._1,t._2)
    }
    def apply(t1 : DataType, t2 : DataType) = instances.apply((t1,t2))
    lazy val Location = prelude.Pair(referenceType,integerType)
  }
  
  object Map {
    object Template extends DomainEnvironment("Map",Seq((loc,"K"),(loc,"V")))  {
      val keyType = factory.makeVariableType(factory.typeVariables.find(_.name == "K").get)(loc)
      val valueType = factory.makeVariableType(factory.typeVariables.find(_.name == "V").get)(loc)
      val dataType = factory.makeNonReferenceDataType(factory,DataTypeSequence(keyType, valueType))(loc)
      
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Constructors
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val empty = factory.defineDomainFunction("empty",DataTypeSequence(),dataType)(loc)
      val update = factory.defineDomainFunction("update",DataTypeSequence(dataType,keyType,valueType),dataType)(loc)
      

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Operations
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val get = factory.defineDomainFunction("get",DataTypeSequence(dataType,keyType),valueType)(loc)
      // ∀ m : Map[K,V], k1,k2 : K, v : V . (k1 ≠ k2 → Get(Update(m,k1,v),k2) = Get(m,k2)) and ((k1 = k2) → Get(Update(m,k1,v),k2) = v)
      factory.addDomainAxiom("get_update",∀(dataType,keyType,keyType,valueType,(m,k1,k2,v) =>
        (
          (not((k1 ≡ k2)) → (get(update(m,k1,v),k2) ≡ get(m,k2))) and
          (    (k1 ≡ k2)  → (get(update(m,k1,v),k2) ≡ v))
        )
      ))(loc)

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Predicates
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val has = factory.defineDomainPredicate("has",DataTypeSequence(dataType,keyType))(loc)
      // ∀ k : K . ¬Has(Empty(),k)
      factory.addDomainAxiom("empty_has_no_entries",∀(keyType,k => not(has(empty(),k))))(loc)
      // ∀ m : Map[K,V], k1,k2 : K, v : V  . Has(Update(m,k1,v),k2) ↔ (k1 = k2 or (k1≠k2 and Has(m,k2))
      factory.addDomainAxiom("has_update",∀(dataType,keyType,keyType,valueType,(m,k1,k2,v) =>
          has(update(m,k1,v),k2) ↔ (k1 ≡ k2 or (not(k1 ≡ k2) and has(m,k2)))
        ))(loc)
    }
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Compile Domain
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    case class PreludeDomainInfo protected[Map] (keyType : DataType, valueType : DataType) {
      val domain = programEnvironment.programFactory.makeDomainInstance(Template.factory,DataTypeSequence(keyType,valueType))
      val dataType = programEnvironment.programFactory.makeNonReferenceDataType(Template.factory,DataTypeSequence(keyType,valueType))(loc)

      val empty = domain.functions.find(_.name == "empty").get
      val update = domain.functions.find(_.name == "update").get
      val get = domain.functions.find(_.name == "get").get
      val has = domain.predicates.find(_.name == "has").get
    }
    val instances = new FactoryHashCache[(DataType,DataType),PreludeDomainInfo] {
      protected def construct(t : (DataType, DataType)) = PreludeDomainInfo(t._1,t._2)
    }
    def apply(t1 : DataType, t2 : DataType) = instances.apply((t1,t2))
    lazy val PermissionMap = prelude.Map(prelude.Pair.Location.dataType,permissionType)
  }
}