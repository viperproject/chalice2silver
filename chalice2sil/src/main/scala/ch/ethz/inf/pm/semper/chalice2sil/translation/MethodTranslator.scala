package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import collection.mutable.Stack
import silAST.methods.implementations.BasicBlockFactory
import silAST.source.{SourceLocation, noLocation}
import silAST.types._
import silAST.expressions.util.{ExpressionSequence, PTermSequence, TermSequence}
import silAST.programs.symbols.{Field, ProgramVariable}
import silAST.expressions.terms._
import silAST.expressions._
import silAST.symbols.logical._
import cfg._
import silAST.domains.{DomainInstance, DomainPredicate, Domain, DomainFunction}
import collection.immutable
import immutable.Set
import silAST.methods.{Method, MethodFactory}
import util._

class MethodTranslator(st : ProgramTranslator, method : chalice.Method)
    extends DerivedProgramEnvironment(st)
    with MethodEnvironment
    with ScopeTranslator
    with TypeTranslator { thisMethodTranslator =>

  val methodFactory = programFactory.getMethodFactory(method,fullMethodName(method))
  protected lazy val implementationFactory = {
    methodFactory.addImplementation(method.body.map(astNodeToSourceLocation).headOption.getOrElse(method))
  }

  def cfgFactory = implementationFactory.cfgFactory

  def declareScopedVariable(sourceLocation : SourceLocation, uniqueName : String, dataType : DataType) = {
    implementationFactory.addProgramVariable(sourceLocation, uniqueName,dataType)
  }

  val nameSequence = NameSequence()

  lazy val callToken : TokenStorage = {
    def createField(baseName : String, dataType : DataType) : FieldTranslator = {
      val f = programFactory.defineField(method,baseName,dataType)
      val ft = new FieldTranslator(f,fields.getNextId,this)
      fields.addExternal(ft)
      ft
    }
    val argFields = methodFactory.parameters.map(p => createField(methodFactory.name + p.name,p.dataType)).toList
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

    val oldMap = olds.map(o => o -> createField(getNextName("old"),o.dataType))
      .toMap

    new TokenStorage(this, argFields, oldMap)
  }

  override val programVariables = new DerivedFactoryCache[chalice.Variable,String, ProgramVariable] with AdjustableCache[ProgramVariable] {
    override protected def deriveKey(p : chalice.Variable) = p.UniqueName

    override protected def deriveKeyFromValue(value : ProgramVariable) = value.name

    override protected def construct(p : chalice.Variable) = implementationFactory.addProgramVariable(p,deriveKey(p),translateTypeExpr(p.t))
  }

  override val thisVariable : ProgramVariable = methodFactory.addParameter(method,"this",referenceType)

  /**
    * Adds a basic block to the SIL AST. The specified Chalice block is used as a prototype for Chalice-level properties
    * like local variable scope.
    * @param name The name of the basic block. Optional.
    */
  @deprecated(message = "Add via scope",since = "uri's while loop change")
  def addBasicBlock(name : String = null) = {
    val uniqueName = if(name != null) getNextName(name) else getNextName(name)
    val block = implementationFactory.cfgFactory.addBasicBlock(uniqueName)(noLocation)
    //return block
    block
  }

  override val temporaries = new TemporaryVariableBroker(this)

  val blockStack = new Stack[BasicBlockFactory]
  def currentExpressionFactory = blockStack.headOption.getOrElse(methodFactory)
  
  var currentChaliceBlock : ChaliceBlock = null

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////      TRANSLATION                                                     /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private[this] def createSignature() = {
    val mf = methodFactory
    method.ins.foreach(i => programVariables.addExternal(mf.addParameter(i, i.UniqueName, translateTypeExpr(i.t))))
    method.outs.foreach(o => programVariables.addExternal(mf.addResult(o,o.UniqueName,translateTypeExpr(o.t))))
    val k = mf.addParameter(method,getNextName("k"),permissionType)
    programVariables.addExternal(k)

    val kTerm = mf.makeProgramVariableTerm(method,k)
    // requires (noPermission < k ∧ k < fullPermission)
    mf.addPrecondition(method,
      mf.makeBinaryExpression (method,And()(method),
        mf.makeDomainPredicateExpression(method,permissionLT,
          TermSequence(currentExpressionFactory.makeNoPermission(method),kTerm)), // noPermission < k
        mf.makeDomainPredicateExpression(method,permissionLT,
          TermSequence(kTerm,currentExpressionFactory.makeFullPermission(method))) // k < writePermission
      )
    )

    val contractTranslator = new DefaultCodeTranslator(this){
      override protected def readFraction(location : SourceLocation) = kTerm
    }

    method.spec.foreach(spec => spec match {
      case chalice.Precondition(e) =>
          val precondition = contractTranslator.translateExpression(e)
          mf.addPrecondition(spec,precondition)
      case chalice.Postcondition(e) =>
          val postcondition = contractTranslator.translateExpression(e)
          mf.addPostcondition(spec,postcondition)
      case otherSpec => report(messages.UnknownAstNode(otherSpec))
    })

    methodFactory.finalizeSignature()

    k
  }
  
  val readFractionVariable = createSignature();

  def translate(){
    translateBody(translateStatements(_,method.body))
  }
}