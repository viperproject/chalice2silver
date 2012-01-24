package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import silAST.types.referenceType
import collection.mutable.Stack
import silAST.methods.implementations.BasicBlockFactory
import silAST.expressions.terms.PTerm
import silAST.source.{SourceLocation, noLocation}
import silAST.symbols.logical.Not
import silAST.expressions.{TrueExpression, Expression}

class MethodTranslator(st : ProgramTranslator, method : chalice.Method) extends MethodEnvironment {
  //Environment
  def programOptions = st.programOptions
  def onNewMessage = st.onNewMessage
  def pastMessages = st.pastMessages

  //ProgramEnvironment
  def programFactory = st.programFactory
  def methodFactories = st.methodFactories
  def fields = st.fields
  
  //MethodEnvironment
  val methodFactory = methodFactories(method)
  def implementationFactory = {
    // TODO: check whether methodFactory.signatureDefined is still false (otherwise throw exception)
    methodFactory.addImplementation(method.body.map(astNodeToSourceLocation).headOption.getOrElse(method))
  }

  def localVariables = FactoryCache(
    (v : chalice.Variable) => implementationFactory.addLocalVariable(v,v.id,silAST.types.referenceType)
  ) // TODO: translate domain types

  def basicBlocks = FactoryCache(
    (n : String) => implementationFactory.addBasicBlock(noLocation,n)
  ) // TODO: get SourceLocation into basic blocks
  
  val blockStack = new Stack[BasicBlockFactory]
  def currentBlock = blockStack.top
 
  // Translation  
  def translate(){
    val mf = methodFactory
    method.ins.foreach(i => mf.addParameter(i,i.id,referenceType)) // TODO: Translate Chalice.Type to SIL type
    method.outs.foreach(o => mf.addResult(o,o.id,referenceType))
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
    blockStack.push(implementationFactory.addBasicBlock(
      method.body.headOption.map(astNodeToSourceLocation).getOrElse(noLocation),
      getNextName("begin_body")
    ))

    translate(method.body)
  }

  def translateAssignment(lhs : chalice.VariableExpr,  rhs : chalice.RValue){
    val v = localVariables(lhs.v)
    currentBlock.appendAssignment(lhs,v,translateTerm(rhs))
  }

  def dummyTerm(location : SourceLocation) = implementationFactory.makeIntegerLiteralTerm(location,27)
  
  def translateTerm(rvalue : chalice.RValue) : PTerm = rvalue match { //RValue is (expression ∪ new-obj)
    //NewRhs  is used for both object creation and channel creation (where lower and upper bounds come into play)
    case chalice.NewRhs(typeId,init,lowerBound,upperBound) =>
      report(messages.UnknownAstNode(rvalue))
      dummyTerm(rvalue)
    // from here on, match against the cases of Expression  
    case chalice.IntLiteral(i) => currentBlock.makeIntegerLiteralTerm(rvalue, i)
    //case chalice.BoolLiteral(b) => // TODO: boolean literal
    case variableExpr:chalice.VariableExpr => currentBlock.makeProgramVariableTerm(variableExpr, localVariables(variableExpr.v))
    case otherNode =>
      report(messages.UnknownAstNode(otherNode))
      dummyTerm(rvalue)
  }
  
  def translate(stmt : chalice.Statement) {stmt match {
    case a@chalice.LocalVar(v,rhsOpt) =>
      rhsOpt match {
        case None => //nothing to do
        case Some(rhs) =>
          val variableExpr = chalice.VariableExpr(v.id)
          variableExpr.v = v
          translateAssignment(variableExpr,rhs)
      }
    case chalice.Assign(variableExpr,rhs) => translateAssignment(variableExpr,rhs)
    case chalice.IfStmt(cond,thn,elsOpt) =>
      val condExpr = translateExpression(cond)

      //Create block for then-branch and the successor block. (else is only created when necessary)
      val thenBlock = basicBlocks(getNextName("if_then"))
      val nextBlock = basicBlocks(getNextName("if_continue"))      

      //Compile then-branch
      //  first, connect to current block via condition
      currentBlock.addSuccessor(stmt,thenBlock,condExpr,false)
      //  then, compile body of then-branch
      blockStack push thenBlock
        translate(thn)
      blockStack pop()
      //  finally, connect then-block to successor with no condition (True)
      thenBlock.addSuccessor(thn,nextBlock,TrueExpression(),false)

      //Handle else-block if there is one. "elseSuccessor" is the block the control should be transferred to
      //  when the condition is false. This is either the actual successor block or the else-branch
      val elseSuccessor = elsOpt match {
        case Some(els) =>
          //Compile else-branch, same as then-branch
          val elseBlock = basicBlocks(getNextName("if_else"))
          blockStack push elseBlock;
            translate(els)
          blockStack pop()

          elseBlock.addSuccessor(els,nextBlock,TrueExpression(),false)
          elseBlock
        case None => nextBlock
      }

      //Create control transfer in case the condition does not hold ↔ ¬condition holds
      currentBlock.addSuccessor(stmt,elseSuccessor,currentBlock.makeUnaryExpression(cond,Not(),condExpr),false)

      //Update currentBlock
      blockStack pop()
      blockStack push nextBlock

    case otherStmt => report(messages.UnknownAstNode(otherStmt))
  }}

  def translate(stmts : Traversable[chalice.Statement]){
    stmts.foreach(translate(_))
  }
  
  def translateExpression(expression : chalice.Expression):Expression = expression match {
    case otherExpr =>
      report(messages.UnknownAstNode(otherExpr))
      dummyExpr(methodFactory,otherExpr) // TODO: currently using method factory instead of implementation factory in order to be able to translate pre-/postconditions where the implementation factory is not available yet.
  }
}