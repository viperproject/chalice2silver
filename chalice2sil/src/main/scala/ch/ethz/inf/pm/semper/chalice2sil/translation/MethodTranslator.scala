package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import collection.mutable.Stack
import silAST.methods.implementations.BasicBlockFactory
import silAST.source.SourceLocation
import silAST.types._
import silAST.programs.symbols.ProgramVariable
import silAST.expressions.terms._
import silAST.expressions._
import silAST.symbols.logical._
import collection.immutable
import immutable.Set
import util._

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
      override protected def mergeMany(rs : Traversable[Set[OldNode]]) = rs.flatten.toSet
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

  val blockStack = new Stack[BasicBlockFactory]
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
      // requires (noPermission < k ∧ k < fullPermission)
      mf.addPrecondition(conjunction(List(
        permissionLT.apply(noPermission,kTerm),
        permissionLT.apply(permissionIntegerMultiplication.apply(1000,kTerm),fullPermission)
      )),method)

      k
    }
  }
  
  val environmentReadFractionVariable = createSignature();
  def environmentReadFractionTerm(sourceLocation : SourceLocation) = currentExpressionFactory.makeProgramVariableTerm(environmentReadFractionVariable,sourceLocation)
  
  private[this] def createContracts() {
    val contractTranslator = new DefaultCodeTranslator(this){
      override protected def readFraction(location : SourceLocation) = environmentReadFractionTerm(location)
    }

    method.spec.foreach(spec => spec match {
      case chalice.Precondition(e) =>
        val precondition = contractTranslator.translateExpression(e)
        methodFactory.addPrecondition(precondition,spec)
      case chalice.Postcondition(e) =>
        val postcondition = contractTranslator.translateExpression(e)
        methodFactory.addPostcondition(postcondition,spec)
      case otherSpec => report(messages.UnknownAstNode(otherSpec))
    })

    methodFactory.finalizeSignature()
  }

  createContracts();

  def translate(){
    translateBody(translateStatements(_,method.body))
  }
}