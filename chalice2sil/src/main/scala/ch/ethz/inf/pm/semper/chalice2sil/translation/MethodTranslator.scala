package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import silAST.methods.implementations.BasicBlockFactory
import silAST.source.SourceLocation
import silAST.types._
import silAST.programs.symbols.ProgramVariable
import silAST.expressions.terms._
import silAST.expressions._
import silAST.symbols.logical._
import collection.immutable
import immutable.Set
import quantification.{LogicalVariable, Forall}
import util._
import silAST.expressions.util.TermSequence
import collection._

class MethodTranslator(st : ProgramTranslator, method : chalice.Method)
    extends DerivedProgramEnvironment(st)
    with MemberEnvironment
    with ScopeTranslator
    with TypeTranslator { thisMethodTranslator =>

  val methodFactory = programFactory.getMethodFactory(fullMethodName(method))(method)
  protected lazy val implementationFactory = {
    methodFactory.addImplementation(method.body.map(astNodeToSourceLocation).headOption.getOrElse(method))
  }

  def cfgFactory = implementationFactory.cfgFactory

  def declareScopedVariable(sourceLocation : SourceLocation, uniqueName : String, dataType : DataType) = {
    implementationFactory.addProgramVariable(uniqueName,dataType)(sourceLocation)
  }

  val nameSequence = NameSequence()

  lazy val callToken : TokenStorage = {
    def createField(baseName : String, dataType : DataType) : FieldTranslator = {
      val f = programFactory.defineField(baseName,dataType)(method)
      val ft = new FieldTranslator(f,fields.getNextId,this)
      fields.addExternal(ft)
      ft
    }
    val argFields = methodFactory.parameters.map(p => createField(methodFactory.name + "::" + p.name,p.dataType)).toList
    val resultFields = methodFactory.results.map(p => createField(methodFactory.name + "::" + p.name,p.dataType)).toList

    val oldFieldEnumerator  = new ExpressionVisitor[Null, immutable.Set[OldNode]] {
      override protected def merge(left : immutable.Set[OldNode], right : immutable.Set[OldNode]) = left union right
      override protected def mergeMany(rs : Traversable[Set[OldNode]]) : Set[OldNode] = rs.flatten.toSet
      override protected def zero = immutable.Set()

      override def visitExpression(expression : Expression, arg : Null) : immutable.Set[OldNode] = expression match {
        case o:OldExpression => immutable.Set(OldExpressionNode(o))
        case _ => super.visitExpression(expression,arg)
      }

      override def visitTerm(term : Term, arg : Null) = term match {
        case o:OldTerm => immutable.Set(OldTermNode(o))
        case _ => super.visitTerm(term,arg)
      }
    }
    val olds : Seq[OldNode] = methodFactory.method.signature.postcondition
      .map(oldFieldEnumerator.visitExpression(_,null)).flatten.toSet.toSeq

    val oldMap = olds.map(o => o -> createField(methodFactory.name + "::" + getNextName("old"),o.dataType))
      .toMap

    new TokenStorage(this, argFields, resultFields, oldMap)
  }

  override val programVariables = new DerivedFactoryCache[chalice.Variable,String, ProgramVariable] with AdjustableCache[ProgramVariable] {
    override protected def deriveKey(p : chalice.Variable) = p.UniqueName

    override protected def deriveKeyFromValue(value : ProgramVariable) = value.name

    override protected def construct(p : chalice.Variable) = implementationFactory.addProgramVariable(deriveKey(p),translateTypeExpr(p.t))(p)
  }

  override val thisVariable : ProgramVariable = methodFactory.addParameter("this",referenceType,method)

  override val temporaries = new TemporaryVariableBroker(this)

  val blockStack = new mutable.Stack[BasicBlockFactory]
  def currentExpressionFactory = blockStack.headOption.getOrElse(methodFactory)

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////      TRANSLATION                                                     /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private[this] def createSignature() : ProgramVariable = {
    val mf = methodFactory
    method.ins.foreach(i => programVariables.addExternal(mf.addParameter(i.UniqueName, translateTypeExpr(i.t),i)))
    method.outs.foreach(o => programVariables.addExternal(mf.addResult(o.UniqueName,translateTypeExpr(o.t),o)))

    pureLanguageConstruct(method){ ctor=>
      import ctor._

      // this pointer
      mf.addPrecondition(Not()(method).t((thisVariable:Term) === nullFunction.apply()),method)

      // read fraction
      val k = mf.addParameter(getNextName("k"),permissionType,method)
      programVariables.addExternal(k)

      val kTerm = mf.makeProgramVariableTerm(k,method)
      // requires (noPermission < k ∧ 1000*k < fullPermission)
      mf.addPrecondition(conjunction(
        permissionLT.apply(noPermission,kTerm),
        permissionLT.apply(permissionIntegerMultiplication.apply(1000,kTerm),fullPermission)
      ),method)

      // thread object
      val currentThread = mf.addParameter(getNextName(prelude.Thread.parameterName),prelude.Thread.dataType,method)
      programVariables.addExternal(currentThread)
      val currentThreadTerm = mf.makeProgramVariableTerm(currentThread,method)
      // requires $CurrentThread != null && acc($CurrentThread.heldMap) && acc($CurrentThread.muMap)
      val heldMapAccess = acc(currentThreadTerm,prelude.Thread.heldMap,fullPermission)
      val muMapAccess = acc(currentThreadTerm,prelude.Thread.muMap,fullPermission)
      mf.addPrecondition(conjunction(
        Not()(method).t((currentThreadTerm:Term) === nullFunction.apply()),
        heldMapAccess,
        muMapAccess
      ),method)
      // ensures acc($CurrentThread.heldMap) && acc($CurrentThread.muMap)
      mf.addPostcondition(conjunction(heldMapAccess,muMapAccess),method)

      // there are more contracts mentioning currentthread in {{createContracts}}

      k
    }
  }
  
  val environmentReadFractionVariable = createSignature()
  def environmentReadFractionTerm(sourceLocation : SourceLocation) = currentExpressionFactory.makeProgramVariableTerm(environmentReadFractionVariable,sourceLocation)

  val environmentCurrentThreadVariable = methodFactory.inputProgramVariables.find(_.name.startsWith(prelude.Thread.parameterName)).get
  def environmentCurrentThreadTerm(sourceLocation : SourceLocation) = currentExpressionFactory.makeProgramVariableTerm(environmentCurrentThreadVariable,sourceLocation)

  private[this] def createContracts() {
    val contractTranslator = new DefaultCodeTranslator(this){
      override protected def readFraction(location : SourceLocation) = environmentReadFractionTerm(location)
    }

    // Add pre- and postconditions
    method.spec.foreach(spec => spec match {
      case chalice.Precondition(e) =>
        val precondition = contractTranslator.translateExpression(e)
        methodFactory.addPrecondition(precondition,spec)
      case chalice.Postcondition(e) =>
        val postcondition = contractTranslator.translateExpression(e)
        methodFactory.addPostcondition(postcondition,spec)
      case chalice.LockChange(es) => {} // handled separately
      case otherSpec => report(messages.UnknownAstNode(otherSpec))
    })

    // Add constraints about currentThread.heldMap and currentThread.muMap
//    (forall o: ref ::   (0<eh[o, held]) == (0<h[o, held])) &&
//    (forall o: ref ::   (0<h[o, held]) ==> eh[o, mu] == h[o, mu]) &&
//    (forall o: ref ::    h[o, held] == eh[o, held])
    def forallReferencesInPostcondition(f : Function[Term,Expression]) = {
      val qObjVar = methodFactory.makeBoundVariable(getNextName("o"),referenceType,method)
      methodFactory.addPostcondition(methodFactory.makeQuantifierExpression(Forall()(method),qObjVar,
        f(currentExpressionFactory.makeBoundVariableTerm(qObjVar,method))
      )(method,Nil),method)
    }

    val lockChanged = method.Spec.collect({case chalice.LockChange(es) => es.map(contractTranslator.translateTerm(_))}).flatten
    def isLockChanged(term : Term) : Expression = {
      lockChanged.map(lc => currentExpressionFactory.makeEqualityExpression(term,lc,lc.sourceLocation):Expression).
        reduceOption((l,r) => currentExpressionFactory.makeBinaryExpression(And()(l.sourceLocation),l,r,l.sourceLocation)).
        getOrElse(FalseExpression()(term.sourceLocation,Nil))
    }
    def exceptIsLockChanged(obj : Term, expr : Expression) = {
      isLockChanged(obj) match {
        case FalseExpression() => expr
        case test =>
          currentExpressionFactory.makeBinaryExpression(Implication()(test.sourceLocation),
            currentExpressionFactory.makeUnaryExpression(Not()(test.sourceLocation),test,test.sourceLocation),
            expr,
            test.sourceLocation)
      }
    }

    val heldMapTerm : Term = currentExpressionFactory.makeFieldReadTerm(environmentCurrentThreadTerm(method),prelude.Thread.heldMap,method)
    def heldLookup(ref : Term) : Term = {
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Map.HeldMap.get,TermSequence(heldMapTerm,ref),ref.sourceLocation)
    }

    val muMapTerm : Term = currentExpressionFactory.makeFieldReadTerm(environmentCurrentThreadTerm(method),prelude.Thread.muMap,method)
    def muLookup(ref : Term) : Term = {
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Map.MuMap.get,TermSequence(muMapTerm,ref),ref.sourceLocation)
    }

    //  ∀ o : ref :: ¬isLockChanged(o) ⇒ old(currentThread.heldMap[o]) == currentThread.heldMap[o]
    forallReferencesInPostcondition(o =>
      exceptIsLockChanged(o,
        currentExpressionFactory.makeEqualityExpression(
          currentExpressionFactory.makeOldTerm(heldLookup(o))(method),
          heldLookup(o),method
        )))

    // ∀ o : ref :: ¬isLockChanged(o) ⇒ (currentThread.heldMap[o] ⇒ old(currentThread.muMap[o]) == currentThread.muMap[o]))
    forallReferencesInPostcondition(o =>
      exceptIsLockChanged(o,
        currentExpressionFactory.makeBinaryExpression(Implication()(method),
          currentExpressionFactory.makeDomainPredicateExpression(prelude.Boolean.eval,TermSequence(heldLookup(o)),method),
          currentExpressionFactory.makeEqualityExpression(currentExpressionFactory.makeOldTerm(muLookup(o))(method,Nil),muLookup(o),method),
          method,Nil
        )
      )
    )

    methodFactory.finalizeSignature()
  }

  createContracts();

  def translate(){
    translateBody(translateStatements(_,method.body))
  }
}