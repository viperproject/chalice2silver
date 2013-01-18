package semper.chalice2sil.translation

import semper.sil.ast.expressions.util.TermSequence
import semper.sil.ast.expressions.Expression
import semper.sil.ast.source.{NoLocation, SourceLocation}
import semper.sil.ast.domains._
import semper.sil.ast.types._
import semper.sil.ast.symbols.logical.quantification.LogicalVariable
import util.{FactoryHashCache, NameSequence}
import semper.sil.ast.expressions.terms.{NoPermissionTerm, FullPermissionTerm, Term}
import scala.Tuple1
import semper.sil.ast.symbols.logical.quantification.Forall

/**
 * Author: Christian Klauser
 */

class ChalicePrelude(programEnvironment : ProgramEnvironment) { prelude =>
  private val names = NameSequence()
  
  private val loc = NoLocation

  protected implicit val unpackNoTypeArguments = (u:Unit) => List[DataType]()
  protected implicit val unpackSingleTypeArgument = (t:Tuple1[DataType]) => List(t._1)
  protected implicit val acceptSingleTypeArgument = (t:DataType) => List(t)
  protected implicit val unpackDoubleTypeArguments = (t:(DataType,DataType)) => List(t._1,t._2)

  protected abstract class DomainEnvironment(
                                                    domainName: String,
                                                    typeVariableNames: Seq[(SourceLocation,String)] = Nil)
      extends DerivedProgramEnvironment(programEnvironment){

    val factory = programFactory.getDomainFactory(domainName, typeVariableNames.map(t => (t._1,t._2,Nil)),loc)

    protected def fApp(domainFunction : DomainFunction, args : Term*) = {
      factory.makeDomainFunctionApplicationTerm(domainFunction,TermSequence(args:_*),loc)
    }
    protected def pApp(domainPredicate : DomainPredicate, args : Term*) = {
      factory.makeDomainPredicateExpression(domainPredicate,TermSequence(args:_*),loc)
    }
    protected def not(operand : Expression) : Expression =
      factory.makeUnaryExpression(semper.sil.ast.symbols.logical.Not()(loc),operand,loc)
    protected def ∀(aType : DataType, expr : LogicalVariable => Expression) : Expression = {
      val a = factory.makeBoundVariable(names.nextName,aType,loc)
      factory.makeQuantifierExpression(Forall()(loc),a,expr(a))(loc)
    }
    protected def ∀(aType : DataType,  bType : DataType,  expr : (LogicalVariable,LogicalVariable) => Expression) : Expression = {
      val a = factory.makeBoundVariable(names.nextName,aType,loc)
      val b = factory.makeBoundVariable(names.nextName,bType,loc)
      factory.makeQuantifierExpression(Forall()(loc),a,
        factory.makeQuantifierExpression(Forall()(loc),b,expr(a,b))(loc)
      )(loc)
    }
    protected def ∀(aType : DataType,  bType : DataType, cType : DataType,  expr : (LogicalVariable,LogicalVariable, LogicalVariable) => Expression) : Expression = {
      val a = factory.makeBoundVariable(names.nextName,aType,loc)
      val b = factory.makeBoundVariable(names.nextName,bType,loc)
      val c = factory.makeBoundVariable(names.nextName,cType,loc)
      factory.makeQuantifierExpression(Forall()(loc),a,
        factory.makeQuantifierExpression(Forall()(loc),b,
          factory.makeQuantifierExpression(Forall()(loc),c,expr(a,b,c))(loc))(loc))(loc)
    }
    protected def ∀(
                     aType : DataType,  
                     bType : DataType, 
                     cType : DataType, 
                     dType : DataType,  
                     expr : (LogicalVariable,LogicalVariable, LogicalVariable,LogicalVariable) => Expression) : Expression = {
      val a = factory.makeBoundVariable(names.nextName,aType,loc)
      val b = factory.makeBoundVariable(names.nextName,bType,loc)
      val c = factory.makeBoundVariable(names.nextName,cType,loc)
      val d = factory.makeBoundVariable(names.nextName,dType,loc)
      factory.makeQuantifierExpression(Forall()(loc),a,
        factory.makeQuantifierExpression(Forall()(loc),b,
          factory.makeQuantifierExpression(Forall()(loc),c,
            factory.makeQuantifierExpression(Forall()(loc),d,expr(a,b,c,d))(loc))(loc))(loc))(loc)
    }
    implicit protected def boundVariableAsTerm(v : LogicalVariable) : Term = factory.makeBoundVariableTerm(v,loc)
    implicit protected def directlyApplyDomainFunction(df : DomainFunction) = new {
      def apply(args : Term*) = fApp(df,args:_*)
    }
    implicit protected def directlyApplyDomainPredicate(dp : DomainPredicate) = new {
      def apply(args : Term*) = pApp(dp,args:_*)
    }

    implicit protected def termOps(lhs : Term) = new {
      def ≡(rhs : Term) =
        factory.makeEqualityExpression(lhs,rhs,loc)
      def ≠(rhs : Term) = not(≡(rhs))
    }
    implicit protected def logicalVariableOps(v : LogicalVariable) = new {
      protected def lhs : Term = v
      def ≡(rhs : Term) =
        factory.makeEqualityExpression(lhs,rhs,loc)
      def ≠(rhs : Term) = not(lhs ≡ rhs)
    }
    implicit protected def exprOps(lhs : Expression) = new {
      // don't use ∧ or ∨ for and/or to take advantage of Scala operator precedence rules.
      def and(rhs : Expression) : Expression =
        factory.makeBinaryExpression(semper.sil.ast.symbols.logical.And()(loc),lhs,rhs,loc)
      def or(rhs : Expression) : Expression =
        factory.makeBinaryExpression(semper.sil.ast.symbols.logical.Or()(loc),lhs,rhs,loc)
      def ↔(rhs : Expression) : Expression =
        factory.makeBinaryExpression(semper.sil.ast.symbols.logical.Equivalence()(loc),lhs,rhs,loc)
      def →(rhs:Expression) : Expression =
        factory.makeBinaryExpression(semper.sil.ast.symbols.logical.Implication()(loc),lhs,rhs,loc)
    }

    implicit protected def integerAsLiteral(n : Int) : Term = factory.makeIntegerLiteralTerm(n,loc)
  }

  abstract class PreludeDomain[TypeArguments]
    (implicit unpackTypeArguments : Function[TypeArguments,Seq[DataType]]) {

    protected def Template : DomainEnvironment

    type DomainInfoType <: DomainInfo

    abstract class DomainInfo(typeArguments : TypeArguments) {
      protected val programFactory = programEnvironment.programFactory
      protected def templateFactory : DomainTemplateFactory = Template.factory

      val domain = programFactory.makeDomainInstance(templateFactory,DataTypeSequence(unpackTypeArguments(typeArguments):_*))
      val dataType = programFactory.makeNonReferenceDataType(templateFactory,DataTypeSequence(unpackTypeArguments(typeArguments):_*),loc)
      protected def function(f : DomainFunction) = domain.functions.find(_.name == f.name).get
      protected def predicate(p : DomainPredicate) = domain.predicates.find(_.name == p.name) .get
    }

    protected def constructDomainInfo(t : TypeArguments) : DomainInfoType

    val instances = new FactoryHashCache[TypeArguments,DomainInfoType] {
      protected def construct(t : TypeArguments) = constructDomainInfo(t)
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

  trait SpecialObjectInfo {
    protected def defineSpecialField(name : String, dataType : DataType) : FieldTranslator = {
      val f = programEnvironment.programFactory.defineField(name, dataType)(loc)
      val ft = new FieldTranslator(f,programEnvironment.fields.getNextId,programEnvironment)
      programEnvironment.fields.addExternal(ft)
      ft
    }

    val dataType = referenceType
  }

  object Object extends SpecialObjectInfo {
    // This special field represents the lock order for each object
    // It's an exact match for the mu field in Chalice.
    // DO NOT CHANGE THE NAME OF THIS FIELD
    lazy val mu : FieldTranslator = defineSpecialField("mu",Mu().dataType)
  }

  object Token extends SpecialObjectInfo {
    // This special fields on tokens indicates whether the token can still be joined.
    // DO NOT CHANGE THE NAME OF THIS FIELD
    lazy val joinable : FieldTranslator = defineSpecialField("joinable",Boolean.dataType)
  }

  object Thread extends SpecialObjectInfo {
    lazy val heldMap = defineSpecialField("$Thread::heldMap",Map(referenceType,Boolean.dataType).dataType)
    lazy val muMap = defineSpecialField("$Thread::muMap",Map(referenceType,Mu().dataType).dataType)
    val parameterName = "$currentThread"
  }
  
  object Predicate extends PreludeDomain[Unit] {
    object Template extends DomainEnvironment("Predicate",List()) {
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Read Fraction
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val globalReadFraction = factory.defineDomainFunction("globalPredicateReadFraction",DataTypeSequence(),permissionType,loc)

      val readFraction = factory.defineDomainFunction("predicateReadFraction",DataTypeSequence(integerType, referenceType),permissionType,loc)
      factory.addDomainAxiom("globalPredicateReadFraction",
        ∀(integerType,referenceType, (pred,ref) => readFraction(pred,ref) ≡ globalReadFraction())
      ,loc)

      // `0 < globalPredicateReadFraction`
      factory.addDomainAxiom("predicateFractionIsReadPermission", pApp(permissionLT,
          NoPermissionTerm()(loc,List()),
          globalReadFraction()),
        loc)

      // `1000*globalPredicateReadFraction < write`
      factory.addDomainAxiom("predicateFractionIsSmall",pApp(permissionLT,
          fApp(permissionIntegerMultiplication,1000,globalReadFraction()),
          FullPermissionTerm()(loc,List())),
        loc)
    }

    class PredicateDomainInfo() extends DomainInfo() {
      val globalReadFraction = function(Template.globalReadFraction)
      val readFraction = function(Template.readFraction)
    }

    protected def constructDomainInfo(t : Unit) = new PredicateDomainInfo()

    type DomainInfoType = PredicateDomainInfo

    protected lazy val instance = instances(())
    def apply() = instance
  }

  object Monitor extends PreludeDomain[Unit]{
    object Template extends DomainEnvironment("Monitor",List()) {
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Read Fraction
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val globalReadFraction = factory.defineDomainFunction("globalMonitorReadFraction",DataTypeSequence(),permissionType,loc)
      factory.addDomainAxiom("monitors_and_predicates_are_the_same",
        Predicate().globalReadFraction() ≡ globalReadFraction(),loc)

      val readFraction = factory.defineDomainFunction("monitorReadFraction",DataTypeSequence(referenceType),permissionType,loc)
      factory.addDomainAxiom("globalMonitorReadFraction",
        ∀(referenceType, (ref) => readFraction(ref) ≡ globalReadFraction())
      ,loc)
    }

    class MonitorDomainInfo() extends DomainInfo() {
      val globalReadFraction = function(Template.globalReadFraction)
      val readFraction = function(Template.readFraction)
    }
    type DomainInfoType = MonitorDomainInfo
    protected def constructDomainInfo(t : Unit) = new MonitorDomainInfo()

    protected lazy val instance = instances(())
    def apply() = instance
  }

  object Function extends PreludeDomain[Unit] {
    object Template extends DomainEnvironment("Function",List()) {
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Read Fraction
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val globalReadFraction = factory.defineDomainFunction("globalFunctionReadFraction",DataTypeSequence(),permissionType,loc)
      factory.addDomainAxiom("functions_and_predicates_are_the_same",
        Predicate().globalReadFraction() ≡ Monitor().globalReadFraction(),loc)

      val readFraction = factory.defineDomainFunction("functionReadFraction",DataTypeSequence(referenceType),permissionType,loc)
      factory.addDomainAxiom("globalFunctionReadFraction",
        ∀(referenceType, (ref) => readFraction(ref) ≡ globalReadFraction())
      ,loc)
    }

    class FunctionDomainInfo() extends DomainInfo() {
      val globalReadFraction = function(Template.globalReadFraction)
      val readFraction = function(Template.readFraction)
    }
    type DomainInfoType = FunctionDomainInfo
    protected def constructDomainInfo(t : Unit) = new FunctionDomainInfo()

    protected lazy val instance = instances(())
    def apply() = instance
  }

  object Pair extends PreludeDomain[(DataType,DataType)] {
    object Template extends DomainEnvironment("Pair",List((loc,"A"),(loc,"B"))){
      val firstType = factory.makeVariableType(factory.typeVariables.find(_.name ==  "A").get,loc)
      var secondType = factory.makeVariableType(factory.typeVariables.find(_.name == "B").get,loc)
      val dataType = factory.makeNonReferenceDataType(factory,DataTypeSequence(firstType,secondType),loc)

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Constructors
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val create = factory.defineDomainFunction("create",DataTypeSequence(firstType,secondType),dataType,loc)

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Operations
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val getFirst =  factory.defineDomainFunction("getFirst",DataTypeSequence(dataType),firstType,loc)
      factory.addDomainAxiom("getFirst",
        ∀(firstType,secondType,(a,b) => getFirst(create(a,b)) ≡ a),loc)

      val getSecond = factory.defineDomainFunction("getSecond",DataTypeSequence(dataType),secondType,loc)
      factory.addDomainAxiom("getSecond",
        ∀(firstType,secondType,(a,b) => getSecond(create(a,b)) ≡ b),loc)

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Axioms
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      factory.addDomainAxiom("equality",∀(firstType,secondType,firstType,secondType,
        (a,b,x,y) => (
          (create(a,b) ≡ create(x,y)) ↔ (a ≡ x and (b ≡ y))
        )),loc)
    }
    class FunctionDomainInfo(t : (DataType,DataType)) extends DomainInfo(t) {
      val create = function(Template.create)
      val getFirst = function(Template.getFirst)
      val getSecond = function(Template.getSecond)
    }
    type DomainInfoType = FunctionDomainInfo
    protected def constructDomainInfo(t : (DataType,DataType)) = new FunctionDomainInfo(t)

    def apply(t1 : DataType, t2 : DataType) = instances.apply((t1,t2))
    lazy val Location = prelude.Pair(referenceType,integerType)
  }
  
  object Map extends PreludeDomain[(DataType,DataType)] {
    object Template extends DomainEnvironment("Map",List((loc,"K"),(loc,"V")))  {
      val keyType = factory.makeVariableType(factory.typeVariables.find(_.name == "K").get,loc)
      val valueType = factory.makeVariableType(factory.typeVariables.find(_.name == "V").get,loc)
      val dataType = factory.makeNonReferenceDataType(factory,DataTypeSequence(keyType, valueType),loc)
      
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Constructors
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val empty = factory.defineDomainFunction("empty",DataTypeSequence(),dataType,loc)
      val update = factory.defineDomainFunction("update",DataTypeSequence(dataType,keyType,valueType),dataType,loc)
      

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Operations
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val get = factory.defineDomainFunction("get",DataTypeSequence(dataType,keyType),valueType,loc)
      // ∀ m : Map[K,V], k1,k2 : K, v : V . (k1 ≠ k2 → Get(Update(m,k1,v),k2) = Get(m,k2)) and ((k1 = k2) → Get(Update(m,k1,v),k2) = v)
      factory.addDomainAxiom("get_update",∀(dataType,keyType,keyType,valueType,(m,k1,k2,v) =>
        (
          (not((k1 ≡ k2)) → (get(update(m,k1,v),k2) ≡ get(m,k2))) and
          (    (k1 ≡ k2)  → (get(update(m,k1,v),k2) ≡ v))
        )
      ),loc)

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Predicates
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val has = factory.defineDomainPredicate("has",DataTypeSequence(dataType,keyType),loc)
      // ∀ k : K . ¬Has(Empty(),k)
      factory.addDomainAxiom("empty_has_no_entries",∀(keyType,k => not(has(empty(),k))),loc)
      // ∀ m : Map[K,V], k1,k2 : K, v : V  . Has(Update(m,k1,v),k2) ↔ (k1 = k2 or (k1≠k2 and Has(m,k2))
      factory.addDomainAxiom("has_update",∀(dataType,keyType,keyType,valueType,(m,k1,k2,v) =>
          has(update(m,k1,v),k2) ↔ (k1 ≡ k2 or (not(k1 ≡ k2) and has(m,k2)))
        ),loc)
    }
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Compile Domain
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    class FunctionDomainInfo(t : (DataType,DataType)) extends DomainInfo(t) {
      val empty = function(Template.empty)
      val update = function(Template.update)
      val get = function(Template.get)
      val has = predicate(Template.has)
    }
    type DomainInfoType = FunctionDomainInfo
    protected def constructDomainInfo(t : (DataType,DataType)) = new FunctionDomainInfo(t)

    def apply(t1 : DataType, t2 : DataType) = instances.apply((t1,t2))
    lazy val PermissionMap = prelude.Map(prelude.Pair.Location.dataType,permissionType)
    lazy val HeldMap = prelude.Map(referenceType,Boolean.dataType)
    lazy val MuMap = prelude.Map(referenceType,Mu().dataType)
  }

  /**
    * The set "Mu" is a dense partially ordered set with a bottom element.
    * It is used to implement deadlock prevention by associating every lock (i.e. every shared object) and
    * every thread with a waitlevel ∈ Mu. A lock may only be taken when the thread's waitlevel is below
    * the lock's mu value.
    */
  object Mu extends PreludeDomain[Unit] {
    object Template extends DomainEnvironment("Mu") {
      val dataType =
        programFactory.makeNonReferenceDataType(factory,DataTypeSequence(),loc,List("Type of Mu elements."))
      val lockBottom = factory.defineDomainFunction("lockBottom",DataTypeSequence(),dataType,loc,List("The bottom element in the partial ordering of the set Mu."))
      val below = factory.defineDomainPredicate("<<",DataTypeSequence(dataType,dataType),loc,List("Determines whether one element of Mu is strictly below another element of Mu."))
      val belowFunc = factory.defineDomainFunction("<<t",DataTypeSequence(dataType,dataType),Boolean.dataType,loc)

      // axioms establishing below as a strict partial ordering of Mu
      factory.addDomainAxiom("irreflexive",∀(dataType,
          (a) => not(below(a,a))
        ),loc)
      factory.addDomainAxiom("asymmetric",∀(dataType,dataType,
          (a,b) => below(a,b) → not(below(b,a))
        ),loc)
      factory.addDomainAxiom("transitive",∀(dataType,dataType,dataType,
        (a,b,c) => (below(a,b) and below(b,c)) → below(a,c)
      ),loc)

      // Make "<<" and "<<t" the same
      factory.addDomainAxiom("below_is_below",∀(dataType,dataType,
        (a,b) => below(a,b) ↔ ((belowFunc(a,b)) ≡ Boolean.trueLiteral())
      ),loc)
      factory.addDomainAxiom("not_below_is_not_below",∀(dataType,dataType,
        (a,b) => not(below(a,b)) ↔ ((belowFunc(a,b)) ≡ Boolean.falseLiteral())
      ),loc)

      // lockBottom is below every non-lockBottom element of Mu
      factory.addDomainAxiom("lockBottom_is_bottom",∀(dataType,
        a => (a ≠ lockBottom()) → below(lockBottom(),a)
      ),loc)
    }
    class MuDomainInfo() extends DomainInfo() {
      val lockBottom = function(Template.lockBottom)
      val below = predicate(Template.below)
    }
    type DomainInfoType = MuDomainInfo
    protected def constructDomainInfo(t : Unit) = new MuDomainInfo()

    protected lazy val instance = instances(())
    def apply() = instance
  }

  object Seq {
    object Template extends DomainEnvironment("Seq",List((loc,"T"))) {
      val elementType = factory.makeVariableType(factory.typeVariables.find(_.name == "T").get,loc)
      val dataType = factory.makeNonReferenceDataType(factory,DataTypeSequence(elementType),loc)

      val length = factory.defineDomainFunction("length",DataTypeSequence(dataType),integerType,loc,List("Computes the length of the list."))
      // axiom ∀ s : Seq[T] . 0 <= length(s)
      factory.addDomainAxiom("length_is_positive",
        ∀(dataType,s => Boolean.eval(integerGE.apply(0,length(s)))),loc)

      val empty = factory.defineDomainFunction("empty",DataTypeSequence(),dataType,loc,List("The empty list constructor."))
      factory.addDomainAxiom("empty_seq_has_length_zero",length(empty()) ≡ 0,loc)
      factory.addDomainAxiom("length_zero_implies_empty_seq",∀(dataType, s => (length(s) ≡ 0) → (s ≡ empty())),loc)

      val singleton = factory.defineDomainFunction("singleton",DataTypeSequence(elementType),dataType,loc,List("Creates a sequence containing a single element."))
      factory.addDomainAxiom("singleton_has_length_one",∀(elementType, t => length(singleton(t)) ≡ 1),loc)

      val append = factory.defineDomainFunction("append",DataTypeSequence(dataType,dataType),dataType,loc,List("Concatenates two sequences."))
      factory.addDomainAxiom("length_of_append_added", ∀(dataType,dataType,(xs,ys) =>
        length(append(xs,ys)) ≡ integerAddition.apply(length(xs),length(ys))
      ),loc)

      val at = factory.defineDomainFunction("at",DataTypeSequence(dataType,integerType),elementType,loc,List("Returns the element stored at the specified index."))
      factory.addDomainAxiom("index_zero_of_singleton",∀(elementType,t => at(singleton(t),0) ≡ t),loc)

      val take = factory.defineDomainFunction("take",DataTypeSequence(dataType,integerType),dataType,loc,List("Returns only the first n elements of a sequence."))
      val drop = factory.defineDomainFunction("drop",DataTypeSequence(dataType,integerType),dataType,loc,List("Returns a sequence without the first n elements."))

    }
  }
}