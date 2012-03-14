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
      factory.makeDDomainFunctionApplicationTerm(loc,domainFunction,DTermSequence(args:_*))
    }
    protected def pApp(domainPredicate : DomainPredicate, args : DTerm*) = {
      factory.makeDDomainPredicateExpression(loc,domainPredicate,DTermSequence(args:_*))
    }
    protected def not(operand : DExpression) : DExpression =
      factory.makeDUnaryExpression(loc,silAST.symbols.logical.Not()(loc),operand)
    protected def and(lhs : DExpression, rhs : DExpression) : DExpression =
      factory.makeDBinaryExpression(loc,silAST.symbols.logical.And()(loc),lhs,rhs)
    protected def or(lhs : DExpression, rhs : DExpression) : DExpression =
      factory.makeDBinaryExpression(loc,silAST.symbols.logical.Or()(loc),lhs,rhs)
    protected def equiv(lhs : DExpression,  rhs : DExpression) : DExpression =
      factory.makeDBinaryExpression(loc,silAST.symbols.logical.Equivalence()(loc),lhs,rhs)
    protected def imply(lhs : DExpression,rhs:DExpression) : DExpression =
      factory.makeDBinaryExpression(loc,silAST.symbols.logical.Implication()(loc),lhs,rhs)
    protected def equality(lhs : DTerm, rhs : DTerm) =
      factory.makeDEqualityExpression(loc,lhs,rhs)
    protected def ∀(aType : DataType, expr : LogicalVariable => DExpression) : DExpression = {
      val a = factory.makeBoundVariable(loc,names.nextName,aType)
      factory.makeDQuantifierExpression(loc,Forall()(loc),a,expr(a))
    }
    protected def ∀(aType : DataType,  bType : DataType,  expr : (LogicalVariable,LogicalVariable) => DExpression) : DExpression = {
      val a = factory.makeBoundVariable(loc,names.nextName,aType)
      val b = factory.makeBoundVariable(loc,names.nextName,bType)
      factory.makeDQuantifierExpression(loc,Forall()(loc),a,
        factory.makeDQuantifierExpression(loc,Forall()(loc),b,expr(a,b))
      )
    }
    protected def ∀(aType : DataType,  bType : DataType, cType : DataType,  expr : (LogicalVariable,LogicalVariable, LogicalVariable) => DExpression) : DExpression = {
      val a = factory.makeBoundVariable(loc,names.nextName,aType)
      val b = factory.makeBoundVariable(loc,names.nextName,bType)
      val c = factory.makeBoundVariable(loc,names.nextName,cType)
      factory.makeDQuantifierExpression(loc,Forall()(loc),a,
        factory.makeDQuantifierExpression(loc,Forall()(loc),b,
          factory.makeDQuantifierExpression(loc,Forall()(loc),c,expr(a,b,c))))
    }
    protected def ∀(
                     aType : DataType,  
                     bType : DataType, 
                     cType : DataType, 
                     dType : DataType,  
                     expr : (LogicalVariable,LogicalVariable, LogicalVariable,LogicalVariable) => DExpression) : DExpression = {
      val a = factory.makeBoundVariable(loc,names.nextName,aType)
      val b = factory.makeBoundVariable(loc,names.nextName,bType)
      val c = factory.makeBoundVariable(loc,names.nextName,cType)
      val d = factory.makeBoundVariable(loc,names.nextName,dType)
      factory.makeDQuantifierExpression(loc,Forall()(loc),a,
        factory.makeDQuantifierExpression(loc,Forall()(loc),b,
          factory.makeDQuantifierExpression(loc,Forall()(loc),c,
            factory.makeDQuantifierExpression(loc,Forall()(loc),d,expr(a,b,c,d)))))
    }
    implicit protected def boundVariableAsTerm(v : LogicalVariable) : DTerm = factory.makeBoundVariableTerm(loc,v)
    implicit protected def directlyApplyDomainFunction(df : DomainFunction) = new {
      def apply(args : DTerm*) = fApp(df,args:_*)
    }
    implicit protected def directlyApplyDomainPredicate(dp : DomainPredicate) = new {
      def apply(args : DTerm*) = pApp(dp,args:_*)
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
    val receiverField = {
      val f = programEnvironment.programFactory.defineField(loc,"receiver",referenceType)
      val t = new FieldTranslator(f,programEnvironment.fields.getNextId,programEnvironment)
      programEnvironment.fields.addExternal(t)
      t
    }
    val readFractionField = {
      val f = programEnvironment.programFactory.defineField(loc,"k",permissionType)
      val t = new FieldTranslator(f,programEnvironment.fields.getNextId,programEnvironment)
      programEnvironment.fields.addExternal(t)
      t
    }

  }

  object Pair {
    object Template extends DomainEnvironment("Pair",Seq((loc,"A"),(loc,"B"))){
      val firstType = factory.makeVariableType(loc,factory.typeVariables.find(_.name ==  "A").get)
      var secondType = factory.makeVariableType(loc,factory.typeVariables.find(_.name == "B").get)
      val dataType = factory.makeNonReferenceDataType(loc,factory,DataTypeSequence(firstType,secondType))

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Constructors
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val create = factory.defineDomainFunction(loc,"create",DataTypeSequence(firstType,secondType),dataType)

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Operations
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val getFirst =  factory.defineDomainFunction(loc,"getFirst",DataTypeSequence(dataType),firstType)
      factory.addDomainAxiom(loc,"getFirst",
        ∀(firstType,secondType,(a,b) => equality(getFirst(create(a,b)),a)))

      val getSecond = factory.defineDomainFunction(loc,"getSecond",DataTypeSequence(dataType),secondType)
      factory.addDomainAxiom(loc,"getSecond",
        ∀(firstType,secondType,(a,b) => equality(getSecond(create(a,b)),b)))

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Axioms
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      factory.addDomainAxiom(loc,"equality",∀(firstType,secondType,firstType,secondType,
        (a,b,x,y) => equiv(
          equality(create(a,b),create(x,y)),
          and(equality(a,x),equality(b,y))
        )))

      lazy val domain = factory.compile()
    }
    case class PreludeDomainInfo protected[Pair] (firstType : DataType, secondType : DataType) {
      val domain = programEnvironment.programFactory.makeDomainInstance(Template.factory,DataTypeSequence(firstType,secondType))
      val dataType = programEnvironment.programFactory.makeNonReferenceDataType(loc,Template.factory,DataTypeSequence(firstType,secondType))

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
      val keyType = factory.makeVariableType(loc,factory.typeVariables.find(_.name == "K").get)
      val valueType = factory.makeVariableType(loc,factory.typeVariables.find(_.name == "V").get)
      val dataType = factory.makeNonReferenceDataType(loc,factory,DataTypeSequence(keyType, valueType))
      
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Constructors
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val empty = factory.defineDomainFunction(loc,"empty",DataTypeSequence(),dataType)
      val update = factory.defineDomainFunction(loc,"update",DataTypeSequence(dataType,keyType,valueType),dataType)
      

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Operations
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val get = factory.defineDomainFunction(loc,"get",DataTypeSequence(dataType,keyType),valueType)
      // ∀ m : Map[K,V], k1,k2 : K, v : V . (k1 ≠ k2 → Get(Update(m,k1,v),k2) = Get(m,k2)) ∧ ((k1 = k2) → Get(Update(m,k1,v),k2) = v)
      factory.addDomainAxiom(loc,"get_update",∀(dataType,keyType,keyType,valueType,(m,k1,k2,v) =>
        and(
          imply(not(equality(k1,k2)), equality(get(update(m,k1,v),k2), get(m,k2))),
          imply(    equality(k1,k2) , equality(get(update(m,k1,v),k2), v))
        )
      ))

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Predicates
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val has = factory.defineDomainPredicate(loc,"has",DataTypeSequence(dataType,keyType))
      // ∀ k : K . ¬Has(Empty(),k)
      factory.addDomainAxiom(loc,"empty_has_no_entries",∀(keyType,k => not(has(empty(),k))))
      // ∀ m : Map[K,V], k1,k2 : K, v : V  . Has(Update(m,k1,v),k2) ↔ (k1 = k2 ∨ (k1≠k2 ∧ Has(m,k2))
      factory.addDomainAxiom(loc,"has_update",∀(dataType,keyType,keyType,valueType,(m,k1,k2,v) =>
        equiv(  has(update(m,k1,v),k2),
          or( equality(k1,k2), and(not(equality(k1,k2)), has(m,k2)))
        )))
    }
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Compile Domain
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    case class PreludeDomainInfo protected[Map] (keyType : DataType, valueType : DataType) {
      val domain = programEnvironment.programFactory.makeDomainInstance(Template.factory,DataTypeSequence(keyType,valueType))
      val dataType = programEnvironment.programFactory.makeNonReferenceDataType(loc,Template.factory,DataTypeSequence(keyType,valueType))

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