package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import silAST.source.noLocation
import silAST.types.referenceType
import silAST.expressions.Expression

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
  def implementationFactory = methodFactory.addImplementation(method.body.map(astNodeToSourceLocation).headOption.getOrElse(method))

  def localVariables = FactoryCache(
    (v : chalice.Variable) => implementationFactory.addLocalVariable(v,v.id,silAST.types.referenceType)
  ) // TODO: translate domain types

  def basicBlocks = FactoryCache(
    (n : String) => implementationFactory.addBasicBlock(noLocation,n)
  ) // TODO: get SourceLocation into basic blocks
  
 
  // Translation  
  def translate(){
    val mf = methodFactory
    method.ins.foreach(i => mf.addParameter(i,i.id,referenceType)) // TODO: Translate Chalice.Type to SIL type
    method.outs.foreach(o => mf.addResult(o,o.id,referenceType))
    method.spec.foreach(spec => spec match {
      case chalice.Precondition(e) =>
        val precondition = translate(e)
        mf.addPrecondition(spec,precondition)
      case chalice.Postcondition(e) =>
        val postcondition = translate(e)
        mf.addPostcondition(spec,postcondition)
      case otherSpec => report(messages.UnknownAstNode(otherSpec))
    })
    
    translate(method.body)
  }

  def translate(stmt : chalice.Statement) {stmt match {
    case a@chalice.LocalVar(v,rhsOpt) =>
    case otherStmt => report(messages.UnknownAstNode(otherStmt))
  }}

  def translate(stmts : Traversable[chalice.Statement]){
    stmts.foreach(translate(_))
  }
  
  def translate(expression : chalice.Expression):Expression = expression match {
    case otherExpr =>
      report(messages.UnknownAstNode(otherExpr))
      dummyExpr(implementationFactory,otherExpr)
  }
}