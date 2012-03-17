package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.programs.symbols.ProgramVariable
import ch.ethz.inf.pm.semper.chalice2sil._
import silAST.source.{noLocation, SourceLocation}
import collection.mutable.Stack
import silAST.expressions.util.TermSequence._
import translation.util._
import silAST.expressions.terms._
import silAST.symbols.logical.{Equivalence, Or, And, Implication}
import silAST.symbols.logical.And._
import silAST.symbols.logical.Implication._
import silAST.expressions._
import silAST.expressions.util.PTermSequence._
import silAST.types._
import silAST.expressions.util.{TermSequence, PTermSequence}
import silAST.methods.implementations.{CFGFactory, BlockFactory, BasicBlockFactory}
import chalice.Variable

/**
  * @author Christian Klauser
  */
trait ScopeTranslator
  extends MethodEnvironment
  with TypeTranslator
{ thisScopeTranslator =>
  def cfgFactory : CFGFactory
  def blockStack : Stack[BasicBlockFactory]
  def temporaries : TemporaryVariableBroker
  def declareScopedVariable(sourceLocation : SourceLocation, uniqueName : String, dataType : DataType) : ProgramVariable

  final type CodeTranslator = ExpressionTranslator with TermTranslator with PermissionTranslator

  class ProgramVariableManager extends DerivedFactoryCache[chalice.Variable,  String, ProgramVariable] {
    protected def deriveKey(p : Variable) = p.UniqueName

    protected def construct(p : Variable) = declareScopedVariable(p,deriveKey(p),translateTypeExpr(p.t))

    override protected def deriveKeyFromValue(value : ProgramVariable) = value.name
  }

  def translateBody[T](body : CodeTranslator => T) {
    val methodEntry = basicBlocks("entry");
    blockStack.push(methodEntry)
    body(new MethodCodeTranslator)
    assert(blockStack.size > 0,"No block on top of the block stack after translation of body.")
    val methodExit = blockStack.pop()

    cfgFactory.setStartNode(methodEntry)
    cfgFactory.setEndNode(methodExit)
    methodExit.setHalt()(noLocation)
  }

  protected def translateStatements(codeTranslator : CodeTranslator, stmts : Traversable[chalice.Statement]){
    stmts.foreach(translateStatement(codeTranslator, _))
  }

  protected def translateStatement(codeTranslator : CodeTranslator, stmt : chalice.Statement) {
    require(!blockStack.isEmpty);
    val oldStackSize = blockStack.length
    val stackTail = blockStack.tail.toSeq

    stmt match {
      case chalice.IfStmt(guard,thn,None) =>
        val guardExpr = translatePExpression(codeTranslator,guard)
        silIf(guardExpr,stmt){
          translateStatement(codeTranslator,thn)
        } end ()
      case chalice.IfStmt(guard,thn,Some(els)) =>
        val guardExpr = translatePExpression(codeTranslator,guard)
        silIf(guardExpr,stmt){
          translateStatement(codeTranslator,thn)
        } els {
          translateStatement(codeTranslator,els)
        } end ()
      case chalice.BlockStmt(ss) => translateStatements(codeTranslator, ss)
      case loopNode@chalice.WhileStmt(condition,_,_,lockChange,body) =>
        val conditionExpr = translatePExpression(codeTranslator,condition)
        //compile loop body
        val loop = cfgFactory.addLoopBlock(getNextName(),conditionExpr)(stmt)
        if(loopNode.Invs != Nil){
          loop.setInvariant(
            loopNode.Invs
              .map(translatePExpression(codeTranslator,_))
              .reduce(currentExpressionFactory.makePBinaryExpression(loopNode,And()(loopNode),_,_)))
        }
        val loopTranslator = new LoopBodyTranslator(this,loop)
        loopTranslator.translateBody(loopTranslator.translateStatement(_,body))

        //integrate loop into CFG
        currentBlock.setGoto(loop)(stmt)
        val nextBlock = basicBlocks(getNextName("after_while"))
        loop.setGoto(nextBlock)(stmt)
        continueWith(nextBlock)
      case a@chalice.LocalVar(v,rhsOpt) =>
        rhsOpt match {
          case None => //nothing to do
          case Some(rhs) =>
            val variableExpr = chalice.VariableExpr(v.UniqueName)
            variableExpr.v = v
            translateAssignment(codeTranslator,variableExpr,rhs)
        }
      case a@chalice.Assert(assertion) =>
        translateAssert(assertion)
      case a@chalice.Assume(assumption) =>
        translateAssume(assumption)
      case chalice.Assign(variableExpr,rhs) => translateAssignment(codeTranslator,variableExpr,rhs)

      case chalice.FieldUpdate(location,rhs) =>
        // rhs could be an object creation. withRhsTranslation makes sure that
        //  the temporary variable used to create the object is released.
        def withRhsTranslation[T](block : PTerm => T) : T = rhs match {
          case newObj:chalice.NewRhs =>
            temporaries.using(referenceType){targetVar =>
              translateNew(codeTranslator,newObj,targetVar)
              block(currentExpressionFactory.makeProgramVariableTerm(newObj,targetVar))
            }
          case expr:chalice.Expression =>
            val t = translatePTerm(codeTranslator, expr)
            block(t)
        }
        def assignViaVar(rcvrVar : ProgramVariable) {
          withRhsTranslation(currentBlock.appendFieldAssignment(stmt, rcvrVar, fields(location.f), _))
        }
        location.e match {
          case rcvr:chalice.VariableExpr => assignViaVar(programVariables(rcvr.v))
          case chalice.ImplicitThisExpr() => assignViaVar(thisVariable)
          case chalice.ExplicitThisExpr() => assignViaVar(thisVariable)
          case rcvr =>
            temporaries.using(referenceType){ rcvrVar =>
              val rcvrTerm = translatePTerm(codeTranslator, rcvr)
              currentBlock.appendAssignment(rcvr,rcvrVar,rcvrTerm)
              withRhsTranslation(currentBlock.appendFieldAssignment(stmt,rcvrVar,fields(location.f),_))
            }
        }
      case callNode:chalice.CallAsync => translateMethodFork(callNode)
      case c:chalice.Call if c.m.isInstanceOf[chalice.Method] =>
        translateMethodCall(c)
      case otherStmt => report(messages.UnknownAstNode(otherStmt))
    }

    assert(blockStack.length == oldStackSize,"translate(chalice.Statement) changed the size of the blockStack when translating %s. Expected %d, actual %d"
      .format(stmt,oldStackSize,blockStack.length))
    assert(blockStack.view.drop(1).sameElements(stackTail),"translate(chalice.Statement) changed the tail elements of the blockStack when translating %s."
      .format(stmt))
  }

  protected abstract sealed class ReadCondition;
  protected final case class ReadImplication(lhs : PExpression, rhs : List[ReadCondition])
    extends ReadCondition
    with Product2[PExpression, List[ReadCondition]] {
    def _1 = lhs
    def _2 = rhs
  }
  protected final case class ReadField(reference : Term, field : FieldTranslator) extends ReadCondition

  /**
    * Produces SIL code that determines the read permission fraction (`k`) for a call (synchronous or asynchronous)
    * This method might introduce new SIL blocks into the CFG.
    * @param codeTranslator The translator to use for the receiver and the arguments.
    * @param callNode The Chalice AST node for the call. Either [[chalice.Call]] or [[chalice.CallAsync]].
    * @return the term that represents the chosen `k`.
    */
  def determineReadPermissionFraction(
                                       codeTranslator : CodeTranslator,
                                       callNode : chalice.Statement { def obj : chalice.Expression;  def args : List[chalice.Expression]; def m : chalice.Callable }) : PTerm = {
    val args = callNode.args;
    val calleeFactory = methods(callNode.m.asInstanceOf[chalice.Method])

    //Read (fractional) permissions
    val readFractionVar = declareScopedVariable(callNode, "k", permissionType) // we won't give it back, ever
    val readFractionTerm = currentExpressionFactory.makeProgramVariableTerm(callNode, readFractionVar)
    // `inhale 0 < k`
    currentBlock.appendInhale(callNode,currentExpressionFactory.makeDomainPredicateExpression(callNode,
      permissionLT,TermSequence(currentExpressionFactory.makeNoPermission(callNode),readFractionTerm)))

    // Permission maps
    // `var m_0 : Map[(ref,int),Permission]`
    val originalPermMapVar = declareScopedVariable(callNode,"m0",prelude.Map.PermissionMap.dataType)
    val originalPermMapTerm = currentExpressionFactory.makeProgramVariableTerm(callNode,originalPermMapVar)
    // `var m : Map[(ref,int),Permission]`
    val permMapVar = declareScopedVariable(callNode,"m",prelude.Map.PermissionMap.dataType)
    val permMapTerm = currentExpressionFactory.makeProgramVariableTerm(callNode,permMapVar)
    // `inhale m = m_0`
    currentBlock.appendInhale(callNode,currentExpressionFactory.makeEqualityExpression(callNode,
      permMapTerm,originalPermMapTerm
    ))

    // Translate arguments and create mapping from parameter variables to these terms
    val argTerms =
      translatePTerm (codeTranslator, callNode.obj) ::
        args.map(translatePTerm(codeTranslator,_)) ++
          List(readFractionTerm)
    val callSubstitution = new ExpressionTransplantation(this) {
      val argMapping = calleeFactory.parameters.zip(argTerms).map(x => x._1 -> x._2).toMap
      def translateProgramVariable(variable : ProgramVariable) = argMapping(variable)
    }
    def transplantExpression(e : Expression) : Expression = {
      callSubstitution.transplant(e)
    }
    def transplantTerm(t : Term) : Term = {
      callSubstitution.transplant(t)
    }

    // Generate assumptions and conditions on fraction
    /**
      * Walks over an [[silAST.expressions.Expression]] and extracts just read permission assertions and implications.
      * @param expr The expression to analyse.
      * @return A list of extracted read conditions.
      */
    def genReadCond(expr : Expression) : List[ReadCondition] = expr match {
      case PermissionExpression(_,_,FullPermissionTerm()) => Nil
      case PermissionExpression(_,_,NoPermissionTerm()) => Nil
      case p@PermissionExpression(reference,field,pTerm) => pTerm match {
        case ProgramVariableTerm(varRef) if varRef == calleeFactory.parameters.last =>

          List(ReadField(transplantTerm(reference),fields.lookup(field.name)))
        case _ =>
          report(messages.PermissionNotUnderstood(callNode,pTerm))
          Nil
      }
      case BinaryExpression(Implication(),lhs,rhs) =>
        //use lhs as-is in implications
        List(ReadImplication(transplantExpression(lhs).asInstanceOf[PExpression],genReadCond(rhs)))
      case BinaryExpression(And(),lhs,rhs) =>
        List(lhs,rhs).map(genReadCond).flatten
      case BinaryExpression(Or(),lhs,rhs) =>
        report(messages.PermissionNotUnderstood(callNode,expr))
        Nil
      case BinaryExpression(Equivalence(),lhs,rhs) =>
        // Interpret A ↔ B ≡ (A → B) ∧ (B → A)
        genReadCond(currentBlock.makeBinaryExpression(
          callNode,
          And()(callNode),
          currentBlock.makeBinaryExpression(callNode,Implication()(callNode),lhs,rhs),
          currentBlock.makeBinaryExpression(callNode,Implication()(callNode),rhs,lhs)
        ))
      case _:AtomicExpression => Nil
      case _ =>
        report(messages.PermissionNotUnderstood(callNode,expr))
        Nil
    }

    /**
      * Takes a list of read permission conditions as extracted by {{genReadCon}} and generates
      * the corresponding conditions on `k`.
      * @param rs the list of read permission conditions to implement.
      */
    def appendCond(rs : List[ReadCondition]){
      rs foreach {
        case ReadField(reference,field) =>
          val originalPermMapTerm = currentExpressionFactory.makeProgramVariableTerm(callNode,originalPermMapVar)
          val permMapTerm = currentExpressionFactory.makeProgramVariableTerm(callNode,permMapVar)
          val readFractionTerm = currentExpressionFactory.makeProgramVariableTerm(callNode,readFractionVar)

          val currentActualPermission = currentExpressionFactory.makePermTerm(callNode,reference,field)
          temporaries.using(prelude.Pair.Location.dataType){ locationVar =>
            val location = currentExpressionFactory.makeProgramVariableTerm(callNode,locationVar)

            // `location := (ref,field)`
            currentBlock.appendAssignment(callNode,locationVar,
              field.locationLiteral(currentExpressionFactory, reference.asInstanceOf[PTerm]))

            // `inhale  get(m_0,(ref,field)) = perm(ref,field)` where (ref,field) = location
            currentBlock.appendInhale(callNode,currentExpressionFactory.makeEqualityExpression(callNode,
              currentExpressionFactory.makeDomainFunctionApplicationTerm(callNode,
                prelude.Map.PermissionMap.get,TermSequence(originalPermMapTerm,location)),
              // ==
              currentActualPermission))

            // `exhale 0 < get(m,(ref,field))`
            val currentVirtualPermission = currentExpressionFactory.makePDomainFunctionApplicationTerm(callNode,
              prelude.Map.PermissionMap.get,PTermSequence(permMapTerm,location))
            currentBlock.appendExhale(callNode,currentExpressionFactory.makeDomainPredicateExpression(callNode,
              permissionLT,TermSequence(currentExpressionFactory.makeNoPermission(callNode),currentVirtualPermission)))

            // `inhale k < get(m,(ref,field))`
            currentBlock.appendInhale(callNode,currentExpressionFactory.makeDomainPredicateExpression(callNode,
              permissionLT,TermSequence(readFractionTerm,currentVirtualPermission)))

            // `m := set(m,(ref,field),get(m,(ref,field)) - k)`
            val nextVirtualPermission = currentExpressionFactory.makePDomainFunctionApplicationTerm(callNode,
              permissionSubtraction,PTermSequence(currentVirtualPermission,readFractionTerm))
            currentBlock.appendAssignment(callNode,permMapVar,currentExpressionFactory.makePDomainFunctionApplicationTerm(callNode,
              prelude.Map.PermissionMap.update,PTermSequence(permMapTerm,location,nextVirtualPermission)))
          }
        case _ =>
      }

      rs collect { case a@ReadImplication(_,_) => a } groupBy (_.lhs) foreach { i =>
        silIf(i._1){
          appendCond(i._2.map(_.rhs).flatten)
        } end()
      }
    }

    calleeFactory.methodFactory.method.signature.precondition
      .map(genReadCond _)
      .foreach(appendCond _)

    readFractionTerm
  }

  /**
    * Translates a single Chalice statement by appending SIL statements to the current block
    * and/or creates edges to new blocks. The number of blocks on the `blockStack` is expected
    * to remain the same, but the top element might change.
    */
  def translateMethodCall(callNode : chalice.Call) {
    val codeTranslator = new MethodCodeTranslator
    val chalice.Call(_,destinations,receiver,_,args) = callNode
    val calleeFactory = methods(callNode.m.asInstanceOf[chalice.Method])

    val readFractionTerm = determineReadPermissionFraction(codeTranslator,callNode)

    // Generate call statement
    val receiverTerm = translatePTerm(codeTranslator,receiver)
    val destinationVars = destinations.map(vExpr => programVariables(vExpr.v))
    val argTerms = args.map(translatePTerm(codeTranslator, _)) ++ List(readFractionTerm)
    currentBlock.appendCall(
      callNode,
      currentBlock.makeProgramVariableSequence(callNode, destinationVars),
      receiverTerm,
      calleeFactory,
      PTermSequence(argTerms : _*))
  }

  def translateMethodFork(callNode : chalice.CallAsync) {
    val codeTranslator = new MethodCodeTranslator
    val chalice.CallAsync(_,tokenVar,receiver,_,args) = callNode
    val calleeFactory = methods(callNode.m)

    // `tok := new object`
    val tokenVersion = programVariables(tokenVar.v)
    currentBlock.appendNew(callNode,tokenVersion,referenceType)

    //Determine read fraction
    val readFractionTerm = determineReadPermissionFraction(codeTranslator, callNode)

    // Store state (arguments, old(*) values)

  }

  def translateAssert(expr : chalice.Expression) {
    val translator = new MethodCodeTranslator with AssertionTranslator
    currentBlock.appendExhale(expr,translator.translateExpression(expr))
  }

  def translateAssume(expr : chalice.Expression) {
    val translator = new MethodCodeTranslator with AssertionTranslator
    currentBlock.appendInhale(expr,translator.translateExpression(expr))
  }

  protected def translateAssignment(codeTranslator : CodeTranslator, lhs : chalice.VariableExpr,  rhs : chalice.RValue){
    val targetVariable = programVariables(lhs.v)

    rhs match {
      //chalice.RValue is (expression ∪ new-obj)
      //NewRhs  is used for both object creation and channel creation (where lower and upper bounds come into play)
      case newObj@chalice.NewRhs(typeId,init,lowerBound,upperBound) =>
        translateNew(codeTranslator,newObj,targetVariable)
      case e:chalice.Expression =>
        val rhsTerm = translatePTerm(codeTranslator, e)
        currentBlock.appendAssignment(lhs,targetVariable,rhsTerm)
    }

    assert(currentBlock.programVariables contains targetVariable,
      "The SIL basic block %s is expected to have the SIL program variable %s in scope. Program variables actually in scope: {%s}"
        .format(currentBlock.name,targetVariable,currentBlock.programVariables.mkString(", ")))
  }

  protected def translateNew(codeTranslator : CodeTranslator, newObj : chalice.NewRhs, targetVar : ProgramVariable) {
    currentBlock.appendNew(newObj,targetVar,referenceType)
    val refTerm = currentExpressionFactory.makeProgramVariableTerm(newObj,targetVar)
    val fullAccess = currentExpressionFactory.makeFullPermission(newObj)
    newObj.typ.Fields foreach { cf =>
      val field = fields(cf)
      currentBlock.appendInhale(newObj,
        currentExpressionFactory.makePermissionExpression(newObj,refTerm,field,fullAccess))
    }
    newObj.initialization foreach  { init =>
      val rhsTerm = translatePTerm(codeTranslator,init.e)
      currentBlock.appendFieldAssignment(init,targetVar,fields(init.f),rhsTerm)
    }
  }

  def translatePTerm(codeTranslator : CodeTranslator, expr : chalice.Expression) : PTerm = {
    codeTranslator.translateTerm(expr) match {
      case pt:PTerm => pt
      case t =>
        assert(false,"Expected program term. Actual type: %s. Location: %s.".format(t.getClass,t.sourceLocation))
        null
    }
  }

  def translatePExpression(codeTranslator : CodeTranslator, expr : chalice.Expression) : PExpression = {
    codeTranslator.translateExpression(expr) match {
      case pe:PExpression => pe
      case t =>
        assert(false,"Expected program expression. Actual type: %s. Location: %s.".format(t.getClass,t.sourceLocation))
        null
    }
  }

  class MethodCodeTranslator extends DefaultCodeTranslator(thisScopeTranslator) {
    override protected def readFraction(location : SourceLocation) =
      currentExpressionFactory.makeProgramVariableTerm(location, readFractionVariable)
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////      HELPER FUNCTIONS (TRANSLATION DSL)                              /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected def silIf[T](cond : PExpression, conditionLocation : SourceLocation = noLocation)(thnBlock : => T) = new {
    def els[U](elsBlock : => U) = new {
      def end() = {
        translateSilCondition[T, U](cond,() => thnBlock,Some(() => elsBlock),Some(conditionLocation))
      }
    }
    def end() = {
      translateSilCondition[T,Unit](cond,() => thnBlock,None,Some(conditionLocation))
    }
  }

  def translateSilCondition[T,U](
                                  condExpr : PExpression,
                                  thn : () => T,
                                  elsOpt : Option[() => U] = None,
                                  thnLoc : Option[SourceLocation] = None,
                                  elsLoc : Option[SourceLocation] = None) : (T, Option[U]) = {

    val thenLocation = thnLoc.getOrElse(noLocation)
    val elseLocation = elsLoc.getOrElse(noLocation)

    //Create block for then-branch. (else is only created when necessary)
    val thenBlock = basicBlocks(getNextName("if_then"))

    //  compile body of then-branch
    val (thenResult,endThenBlock) = into(thenBlock,thn())

    // IMPORTANT: do not create the nextBlock until both branches are compiled
    //  otherwise, temporary variables declared in else might not known!
    lazy val nextBlock = basicBlocks(getNextName("if_continue"))

    //Handle else-block if there is one. "elseSuccessor" is the block the control should be transferred to
    //  when the condition is false. This is either the actual successor block or the else-branch

    val (elseResult,elseSuccessor) = elsOpt match {
      case Some(els) =>
        //Compile else-branch, same as then-branch
        val elseBlock = basicBlocks(getNextName("if_else"))
        val (result,elseBlockEnd) = into(elseBlock,els())

        elseBlockEnd.setGoto(nextBlock)(elseLocation)
        (Some(result),elseBlock)
      case None => (None,nextBlock)
    }

    //  finally, connect then-block to successor with no condition (True)
    endThenBlock.setGoto(nextBlock)(thenLocation)

    //Create control transfer from current block to then or else
    currentBlock.setBranch(condExpr,thenBlock,elseSuccessor)(thenLocation)

    //Update currentBlock
    this.continueWith(nextBlock)

    (thenResult,elseResult)
  }

  def basicBlocks = new FactoryHashCache[String,  BasicBlockFactory]{
    protected def construct(key : String) = cfgFactory.addBasicBlock(key)(noLocation)
  }

  def currentBlock = {
    require(!blockStack.isEmpty,"Attempted to access \"current block\" outside of method body.")
    blockStack.top
  }

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
    (result,blockEnd)
  }

  protected def continueWith(block : BasicBlockFactory) = {
    require(!blockStack.isEmpty)

    blockStack.pop()
    blockStack.push(block)
  }
}
