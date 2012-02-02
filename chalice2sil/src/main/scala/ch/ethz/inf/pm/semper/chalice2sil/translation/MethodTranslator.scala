package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import collection.mutable.Stack
import silAST.methods.implementations.BasicBlockFactory
import silAST.source.{SourceLocation, noLocation}
import silAST.expressions.{TrueExpression, Expression}
import silAST.programs.symbols.ProgramVariable
import silAST.symbols.logical.{Or, And, Not}
import silAST.types.{DataType, NonReferenceDataType, referenceType}
import silAST.expressions.util.TermSequence
import silAST.domains.{DomainPredicate, Domain, DomainFunction}
import silAST.expressions.terms.{noPermissionTerm, fullPermissionTerm, Term, PTerm}

class MethodTranslator(st : ProgramTranslator, method : chalice.Method) extends MethodEnvironment {
  //Environment
  def programOptions = st.programOptions
  def onNewMessage = st.onNewMessage
  def pastMessages = st.pastMessages

  //ProgramEnvironment
  def programFactory = st.programFactory
  def methodFactories = st.methodFactories
  def fields = st.fields
  def prelude = st.prelude

  //MethodEnvironment
  val methodFactory = methodFactories(method)
  lazy val implementationFactory = {
    //requires(!methodFactory.isSignatureDefined)
    methodFactory.addImplementation(method.body.map(astNodeToSourceLocation).headOption.getOrElse(method))
  }

  val localVariables = new AdjustableFactoryHashCache[String, ProgramVariable] with DerivedFactoryCache[chalice.Variable,String, ProgramVariable] {
    def construct(name : String) = implementationFactory.addLocalVariable(noLocation,name,silAST.types.referenceType)
    def deriveKey(variable : chalice.Variable) = variable.id
    def getKeyFor(variable : ProgramVariable)  = variable.name
  }

  val basicBlocks = new AdjustableFactoryHashCache[String, BasicBlockFactory] {
    def construct(name : String) = implementationFactory.addBasicBlock(noLocation,name)
    def getKeyFor(block : BasicBlockFactory) = block.name
  }

  val temporaries = new TemporaryVariableBroker(this)
  
  val blockStack = new Stack[BasicBlockFactory]
  def currentBlock = {
    require(!blockStack.isEmpty,"Attempted to access \"current block\" outside of method body.")
    blockStack.top
  }
  def currentExpressionFactory = blockStack.headOption.getOrElse(methodFactory)
 
  // Translation  
  def translate(){
    val mf = methodFactory
    // TODO: Translate Chalice.Type to SIL type
    method.ins.foreach(i => localVariables.addExternal(mf.addParameter(i, i.id, translate(i.t))))
    method.outs.foreach(o => localVariables.addExternal(mf.addResult(o,o.id,translate(o.t))))
    method.spec.foreach(spec => spec match {
      case chalice.Precondition(e) =>
        val precondition = translateExpression(e)
        mf.addPrecondition(spec,precondition)
      case chalice.Postcondition(e) =>
        val postcondition = translateExpression(e)
        mf.addPostcondition(spec,postcondition)
      case otherSpec => report(messages.UnknownAstNode(otherSpec))
    })

    methodFactory.finalizeSignature()

    // SILAST requires the first and last block to be created separately.
    //  The first block can be used and is supplied as the block to translate into
    //  The last block, however, cannot be anticipated. We just add an edge from the
    //    exit block of the actual (translated) body to the last block.
    val firstBody = implementationFactory.addFirstBasicBlock(method,getNextName("begin_body"))
    basicBlocks.addExternal(firstBody)

    val (_,exitBody) = into(firstBody,translate(method.body))

    val lastBlock = implementationFactory.addLastBasicBlock(method,getNextName("exit_body"))
    basicBlocks.addExternal(lastBlock)

    exitBody.addSuccessor(method,lastBlock,TrueExpression(),false)
  }

  def translateAssignment(lhs : chalice.VariableExpr,  rhs : chalice.RValue){
    val v = localVariables.getFromPrototype(lhs.v)
    currentBlock.appendAssignment(lhs,v,translateTerm(rhs).asInstanceOf[PTerm]) //TODO: is there a way to avoid this cast without duplicating translateTerm?
  }

  def dummyTerm(location : SourceLocation) = currentExpressionFactory.makeIntegerLiteralTerm(location,27)

  /**
    * Runs a translation with the specified block as the currentBlock. Will then ensure that
    * the block that ends up as the currentBlock at the end of the translation does not have any
    * outgoing edges already, i.e., is open for additional statements.
    *
    * The typical pattern for using `into` is as follows:
    * {{{
    * val targetBlockBegin = basicBlocks(getNextName("target_block"))
    * val (result,targetEndBlock) = into(targetBlockBegin, myTranslation(x))
    * }}}
    * At this point `targetBlockBegin` and `targetBlockEnd` might be the same block, or there might be
    * an arbitrary number of block in between.
    *
    * The end block is guaranteed not to have any outgoing edges, so you can safely append statements
    * that are meant to follow `myTranslation(x)`.
    */
  protected def into[T](blockHead : BasicBlockFactory, translation : =>  T) : (T,BasicBlockFactory) = {
    blockStack push blockHead
    val result = translation
    val blockEnd = blockStack.pop()
    assert(!hasOutgoingEdges(blockEnd),"The block \"%s\" is expected to not have any outgoing edges. %s".format(blockEnd.name, blockEnd.compile()))
    (result,blockEnd)
  }
  
  protected def continueWith(block : BasicBlockFactory) = {
    require(!blockStack.isEmpty)
    require(!hasOutgoingEdges(block))
    blockStack pop  ()
    blockStack push block
  }
  
  protected def hasOutgoingEdges(block : BasicBlockFactory) : Boolean = !block.compile().successors.isEmpty
  
  def translatePTerm(rvalue : chalice.RValue) : PTerm = {
    translateTerm(rvalue) match {
      case pt:PTerm => pt
      case t =>
        assert(false,"Expected program term. Actual type: %s. Location: %s.".format(t.getClass,t.sourceLocation))
        null
    }
  }
  
  def translateTerm(rvalue : chalice.RValue) : Term = {
    rvalue match { //RValue is (expression ∪ new-obj)
      //NewRhs  is used for both object creation and channel creation (where lower and upper bounds come into play)
      case chalice.NewRhs(typeId,init,lowerBound,upperBound) =>
        report(messages.UnknownAstNode(rvalue))
        dummyTerm(rvalue)
      // from here on, match against the cases of Expression  
      case chalice.IntLiteral(i) => currentExpressionFactory.makeIntegerLiteralTerm(rvalue, i)
      //case chalice.BoolLiteral(b) => // TODO: boolean literal
      case variableExpr:chalice.VariableExpr => currentExpressionFactory.makeProgramVariableTerm(variableExpr, localVariables.getFromPrototype(variableExpr.v))
      case chalice.Old(e) => currentExpressionFactory.makeOldTerm(rvalue,translateTerm(e))
      case access@chalice.MemberAccess(rcvr,_) if !access.isPredicate =>
        assert(access.f != null,"Chalice MemberAccess node (%s) is not linked to a field.".format(access))
        currentExpressionFactory.makeFieldReadTerm(access,translateTerm(rcvr),fields(access.f))
      case th@chalice.ImplicitThisExpr() => currentExpressionFactory.makeProgramVariableTerm(th,methodFactory.thisVar)
      case th@chalice.ExplicitThisExpr() => currentExpressionFactory.makeProgramVariableTerm(th,methodFactory.thisVar)
      case otherNode =>
        report(messages.UnknownAstNode(otherNode))
        dummyTerm(rvalue)
    }
  }

  def translateCondition(cond : chalice.Expression, thn : chalice.Statement, elsOpt : Option[chalice.Statement]) : Unit = {
    val elsTransOpt = elsOpt.map((e) => () => translate(e))
    val thnTrans = () => translate(thn)
    translateCondition(cond,thnTrans,elsTransOpt,Some(thn),elsOpt.map(astNodeToSourceLocation))
  }
  
  def translateCondition(cond : chalice.Expression,  thn : chalice.Statement) : Unit = {
    val thnTrans = () => translate(thn)
    val (result,_) = translateCondition(cond, thnTrans, None)
    result
  }

  def translateCondition[T,U](cond : chalice.Expression,
                              thn : () => T,
                              elsOpt : Option[() => U]) : (T,Option[U]) = {
    translateCondition(cond,thn,elsOpt,None,None)
  }
  
  def translateCondition[T,U](
                      cond : chalice.Expression,
                      thn : () => T,
                      elsOpt : Option[() => U],
                      thnLoc : Option[SourceLocation],
                      elsLoc : Option[SourceLocation]) : (T, Option[U]) = {
    
    val condExpr = translateExpression(cond)

    //Create block for then-branch and the successor block. (else is only created when necessary)
    val thenBlock = basicBlocks(getNextName("if_then"))
    val nextBlock = basicBlocks(getNextName("if_continue"))

    //Compile then-branch
    //  first, connect to current block via condition
    currentBlock.addSuccessor(thnLoc.getOrElse(cond),thenBlock,condExpr,false)
    //  then, compile body of then-branch
    val (thenResult,endThenBlock) = into(thenBlock,thn())
    //  finally, connect then-block to successor with no condition (True)
    endThenBlock.addSuccessor(thnLoc.getOrElse(cond),nextBlock,TrueExpression(),false)

    //Handle else-block if there is one. "elseSuccessor" is the block the control should be transferred to
    //  when the condition is false. This is either the actual successor block or the else-branch
    val (elseResult,elseSuccessor) = elsOpt match {
      case Some(els) =>
        //Compile else-branch, same as then-branch
        val elseBlock = basicBlocks(getNextName("if_else"))
        val (result,elseBlockEnd) = into(elseBlock,els())

        elseBlockEnd.addSuccessor(elsLoc.getOrElse(cond),nextBlock,TrueExpression(),false)
        (Some(result),elseBlock)
      case None => (None,nextBlock)
    }

    //Create control transfer in case the condition does not hold ↔ ¬condition holds
    currentBlock.addSuccessor(cond,elseSuccessor,currentBlock.makeUnaryExpression(cond,Not(),condExpr),false)

    //Update currentBlock
    this continueWith nextBlock

    (thenResult,elseResult)
  }

  /**
    * Translates a single Chalice statement by appending SIL statements to the current block
    * and/or creates edges to new blocks. The number of blocks on the `blockStack` is expected
    * to remain the same, but the top element might change.
    */
  def translate(stmt : chalice.Statement) {
    require(!blockStack.isEmpty); 
    val oldStackSize = blockStack.length
    val stackTail = blockStack.tail.toSeq
    
    stmt match {
      case chalice.BlockStmt(body) => translate(body)
      case a@chalice.LocalVar(v,rhsOpt) =>
        rhsOpt match {
          case None => //nothing to do
          case Some(rhs) =>
            val variableExpr = chalice.VariableExpr(v.id)
            variableExpr.v = v
            translateAssignment(variableExpr,rhs)
        }
      case chalice.Assign(variableExpr,rhs) => translateAssignment(variableExpr,rhs)
      case chalice.IfStmt(cond,thn,elsOpt) => translateCondition(cond,thn,elsOpt)
      case chalice.FieldUpdate(location,rhs) =>
        def assignViaVar(rcvrVar : ProgramVariable){
          currentBlock.appendFieldAssignment(stmt,rcvrVar,fields(location.f),translatePTerm(rhs))
        }
        location.e match {
          case rcvr:chalice.VariableExpr => assignViaVar(localVariables(rcvr.v.id))
          case chalice.ImplicitThisExpr() => assignViaVar(methodFactory.thisVar)
          case rcvr =>
            temporaries.using(referenceType, rcvrVar => {
              currentBlock.appendAssignment(rcvr,rcvrVar,translatePTerm(rcvr))
              currentBlock.appendFieldAssignment(stmt,rcvrVar,fields(location.f),translatePTerm(rhs))
            })
        }
      case otherStmt => report(messages.UnknownAstNode(otherStmt))
    }
    
    assert(blockStack.length == oldStackSize,"translate(chalice.Statement) changed the size of the blockStack when translating %s. Expected %d, actual %d"
      .format(stmt,oldStackSize,blockStack.length))
    assert(blockStack.view.drop(1).sameElements(stackTail),"translate(chalice.Statement) changed the tail elements of the blockStack when translating %s."
      .format(stmt))
  }

  def translate(stmts : Traversable[chalice.Statement]){
    stmts.foreach(translate(_))
  }
  
  def translateExpression(expression : chalice.Expression):Expression = expression match {
    case chalice.And(lhs,rhs) => 
      val lhsT = translateExpression(lhs)
      val rhsT = translateExpression(rhs)
      currentExpressionFactory.makeBinaryExpression(expression,And(),lhsT,rhsT)
    case chalice.Or(lhs,rhs) =>
      val lhsT = translateExpression(lhs)
      val rhsT = translateExpression(rhs)
      currentExpressionFactory.makeBinaryExpression(expression,Or(),lhsT,rhsT)
    case binary:chalice.BinaryExpr => 
      val (lhs,rhs) = (binary.E0,binary.E1)
      // binary.ExpectedXhsType is often null, use the "inferred" types for the operands instead
      val (lhsType,rhsType,resultType) = (translate(lhs.typ),translate(rhs.typ),translate(binary.ResultType))
      // TODO: ensure that resultType is a boolean value, otherwise report messages.TermInExpressionPosition

      // TODO: resolve operators via type-directed lookup ("domain-directed")
      // search the three involved types/domains (lhs,rhs,result) and verify that the operator has the correct signature
      def findOperator(d:Domain):Option[DomainPredicate] = {
        d.predicates.find(p =>
          p.name == binary.OpName
            && p.signature.argumentTypes.length == 2
            && p.signature.argumentTypes.view
                .zip(List(lhsType,rhsType))
                .forall(((sig : DataType,act : DataType) => sig.isCompatible(act)).tupled)
            )
      }
      List(lhsType,rhsType,resultType)
        .collect({case NonReferenceDataType(_,d) => d})
        .map(findOperator).collect({case Some(x) => x}).headOption match {
        case Some(opPred) =>
          currentExpressionFactory.makeDomainPredicateExpression(binary,opPred,TermSequence(translateTerm(lhs),translateTerm(rhs)))
        case None => 
          report(messages.OperatorNotFound(binary,lhsType,rhsType,resultType))
          dummyExpr(currentExpressionFactory,binary)
      }
    case chalice.Access(memberAccess, permission) =>
      currentExpressionFactory.makePermissionExpression(expression,translateTerm(memberAccess.e),fields(memberAccess.f),translatePermission(permission))
    case otherExpr =>
      report(messages.UnknownAstNode(otherExpr))
      dummyExpr(currentExpressionFactory,otherExpr) // TODO: currently using method factory instead of implementation factory in order to be able to translate pre-/postconditions where the implementation factory is not available yet.
  }
  
  protected def translatePermission(permission : chalice.Permission) : Term = permission match {
    case chalice.Full => fullPermissionTerm
    case _ =>
      report(messages.UnknownAstNode(permission))
      noPermissionTerm
  }

  protected def translate(typeExpr : chalice.Type) = new TypeTranslator(this).translate(typeExpr)

  protected def translate(classRef : chalice.Class) = new TypeTranslator(this).translate(classRef)
}