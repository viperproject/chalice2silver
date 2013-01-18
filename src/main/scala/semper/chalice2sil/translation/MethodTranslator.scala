package semper.chalice2sil.translation

import semper.chalice2sil
import chalice2sil._
import semper.sil.ast.methods.implementations.BasicBlockFactory
import semper.sil.ast.source.SourceLocation
import semper.sil.ast.types._
import semper.sil.ast.programs.symbols.ProgramVariable
import semper.sil.ast.expressions.terms._
import semper.sil.ast.expressions._
import semper.sil.ast.symbols.logical._
import util._
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
      override protected def mergeMany(rs : Traversable[immutable.Set[OldNode]]) : immutable.Set[OldNode] = rs.flatten.toSet
      override protected def zero = immutable.Set()

      override def visitExpression(expression : Expression, arg : Null) : immutable.Set[OldNode] = expression match {
        case o:OldExpression => immutable.Set(OldExpressionNode(o))
        case _ => super.visitExpression(expression,arg)
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
      mf.addPrecondition(Not()(method).t((thisVariable:Expression) === nullFunction.apply()),method)

      // read fraction
      val k = mf.addParameter(getNextName("k"),permissionType,method)
      programVariables.addExternal(k)

      val kExpression = mf.makeProgramVariableExpression(k,method)
      // requires (noPermission < k ∧ 1000*k < fullPermission)
      mf.addPrecondition(conjunction(
        permissionLT.apply(noPermission,kExpression),
        permissionLT.apply(permissionIntegerMultiplication.apply(1000,kExpression),fullPermission)
      ),method)

      // thread object
      val currentThread = mf.addParameter(getNextName(prelude.Thread.parameterName),prelude.Thread.dataType,method)
      programVariables.addExternal(currentThread)

      // contracts mentioning CurrentThread are located in createContracts below

      k
    }
  }
  
  val environmentReadFractionVariable = createSignature()

  val environmentCurrentThreadVariable = methodFactory.inputProgramVariables.find(_.name.startsWith(prelude.Thread.parameterName)).get

  private[this] def createContracts() {
    val contractTranslator = new DefaultCodeTranslator(this){
      override protected def readFraction(location : SourceLocation) = environmentReadFractionExpression(location)
    }

    val location : SourceLocation = method

    // Step 1: Create pre- and postconditions for $CurrentThread
    //  This needs to happen *before* the user's pre- and postconditions are handled,
    //  because they might refer to $CurrentThread.muMap and $CurrentThread.heldMap.
    //  Otherwise, we wouldn't have access to these fields.
    pureLanguageConstruct(location){ ctor =>
      import ctor._

      val currentThreadExpression = environmentCurrentThreadExpression(location)
      methodFactory.addPrecondition(conjunction(
        Not()(method).t((currentThreadExpression : Expression) === nullFunction.apply()),
        acc(currentThreadExpression, prelude.Thread.heldMap, fullPermission),
        acc(currentThreadExpression, prelude.Thread.muMap, fullPermission)
      ), method)

      // Create postcondition for $CurrentThread
      //  (the same specification is used for loop invariants)
      val heldMapExpression : Expression = currentThreadExpression!prelude.Thread.heldMap
      val muMapExpression : Expression = currentThreadExpression!prelude.Thread.muMap

      val lockChanged : List[Expression] = method.Spec.collect({
        case chalice.LockChange(es) => es.map(contractTranslator.translateExpression(_))
      }).flatten

      val oldMuMap = currentExpressionFactory.makeOldExpression(muMapExpression)(location, Nil)
      val oldHeldMap = currentExpressionFactory.makeOldExpression(heldMapExpression)(location,Nil)

      generateThreadInvariants(location, lockChanged, oldHeldMap, oldMuMap) foreach {x =>
        methodFactory.addPostcondition(x._1,x._2)
      }
    }

    // Add pre- and postconditions from original Chalice source code
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

    methodFactory.finalizeSignature()

    // Verify that no old(*) expressions appear inside ∀ or ∃ quantifiers. Chalice2SIL cannot deal with that
    methodFactory.method.signature
      .postcondition
      .map(util.ContractFormChecker(_))
      .reduceOption(_ ++ _)
      .getOrElse(Set())
      .foreach(report(_))
  }

  createContracts()

  def translate(){
    translateBody(translateStatements(_,method.body))
  }
}