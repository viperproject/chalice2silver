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
import ssa._
import silAST.methods.MethodFactory
import silAST.domains.{DomainInstance, DomainPredicate, Domain, DomainFunction}
import support.{MoveToBlock, ExpressionTransplantation, TemporaryVariableHost, TemporaryVariableBroker}

class MethodTranslator(st : ProgramTranslator, method : chalice.Method)
    extends DerivedProgramEnvironment(st)
    with MethodEnvironment
    with TemporaryVariableHost
    with TypeTranslator { thisMethodTranslator =>
  //MethodEnvironment
  val methodFactory = programFactory.getMethodFactory(method,fullMethodName(method))
  override lazy val implementationFactory = {
    methodFactory.addImplementation(method.body.map(astNodeToSourceLocation).headOption.getOrElse(method))
  }

  val nameSequence = NameSequence()

  override val programVariables = new DerivedFactoryCache[ssa.Version,String, ProgramVariable] with AdjustableCache[ProgramVariable] {
    override protected def deriveKey(p : ssa.Version) = p.uniqueName

    override protected def deriveKeyFromValue(value : ProgramVariable) = value.name

    override protected def construct(p : ssa.Version) = implementationFactory.addLocalVariable(p.chaliceVariable,deriveKey(p),translateTypeExpr(p.chaliceVariable.t))
  }

  override val thisVariable = methodFactory.thisVar

  def localVariableVersion(variable : chalice.Variable) = {
    if(currentAssignmentInterpretation == null && (method.ins.contains(variable) || method.outs.contains(variable))){
      programVariables.lookup(variable.UniqueName)
    } else {
      require(currentAssignmentInterpretation != null,
        "Tried to access local variable version of variable %s without an assignment interpretation.".format(variable))
      programVariables(currentAssignmentInterpretation.version(variable))
    }
  }

  override val basicBlocks = new AdjustableFactoryHashCache[String, BasicBlockFactory] {
    protected override def construct(name : String) : BasicBlockFactory = {
      require(false,"Cannot create basic blocks this way.") //TODO: adjust MethodEnvironment
      null
    }
    protected override def getKeyFor(block : BasicBlockFactory) = block.name
  }

  /**
    * Adds a basic block to the SIL AST. The specified Chalice block is used as a prototype for Chalice-level properties
    * like local variable scope.
    * @param chaliceBlock The Chalice block to use as a prototype for the new basic block.
    * @param name The name of the basic block. Optional.
    */
  def addBasicBlock(chaliceBlock : ChaliceBlock, name : String = null) = {
    val uniqueName = if(name != null) getNextName(chaliceBlock.name + "_" + name) else getNextName(chaliceBlock.name)
    val block = implementationFactory.addBasicBlock(chaliceBlock.sourceLocation,uniqueName)

    inheritChaliceBlockProperties(chaliceBlock, block)

    //return block
    block
  }
  
  def inheritChaliceBlockProperties(chaliceBlock : ChaliceBlock, block : BasicBlockFactory){
    // variable scope
    chaliceBlock.versionsInScope foreach { v =>
      val pv = programVariables(v)
      if(!((methodFactory.parameters contains pv) || (methodFactory.results contains pv))){
        block.addProgramVariableToScope(programVariables(v))
      }
    }
    chaliceBlock.temporariesInScope foreach  { v =>
      block.addProgramVariableToScope(v)
    }
  }

  override val temporaries = new TemporaryVariableBroker(this)

  def allocateLocallyScoped(sourceLocation : SourceLocation, name : String, dataType : DataType) : ProgramVariable = {
    var v = implementationFactory.addLocalVariable(sourceLocation, getNextName(name),dataType)
    bringTemporaryVariableIntoScope(v)
    v
  }

  def bringTemporaryVariableIntoScope(v : ProgramVariable) {
    currentChaliceBlock.temporariesInScope += v
    if(!currentBlock.localVariables.contains(v)){
      currentBlock.addProgramVariableToScope(v)
    }
  }

  val blockStack = new Stack[BasicBlockFactory]
  def currentBlock = {
    require(!blockStack.isEmpty,"Attempted to access \"current block\" outside of method body.")
    blockStack.top
  }
  def currentExpressionFactory = blockStack.headOption.getOrElse(methodFactory)
  
  var currentChaliceBlock : ChaliceBlock = null
  var currentAssignmentInterpretation : AssignmentInterpretation = null

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////      TRANSLATION                                                     /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  val cfg : ControlFlowSketch = {
    val ssa = new SsaSurvey(this, nameSequence)
    val cfg = ssa.translateControlFlow(method)
    ssa.determineDominators(cfg)
    ssa.determineDominanceFrontiers(cfg)
    ssa.determineΦLocations(cfg)
    ssa.determineIntermediateVersions(cfg)
    ssa.determineDefinitionReach(cfg)
    ssa.determineVariableScopes(cfg,method.ins ++ method.outs)
    
    cfg
  }

  private[this] def createSignature() = {
    val mf = methodFactory
    method.ins.foreach(i => programVariables.addExternal(mf.addParameter(i, i.UniqueName, translateTypeExpr(i.t))))
    method.outs.foreach(o => programVariables.addExternal(mf.addResult(o,o.UniqueName,translateTypeExpr(o.t))))
    val k = mf.addParameter(method,getNextName("k"),permissionType)
    programVariables.addExternal(k)

    val kTerm = mf.makeProgramVariableTerm(method,k)
    // requires (noPermission < k ∧ k < fullPermission)
    mf.addPrecondition(method,
      mf.makeDomainPredicateExpression(method,permissionLT,
        TermSequence(currentExpressionFactory.makeNoPermission(method),kTerm)) // noPermission < k
    )

    val contractTranslator = new DefaultCodeTranslator(this){
      override protected def readFraction(location : SourceLocation) = kTerm
    }

    method.spec.foreach(spec => spec match {
      case chalice.Precondition(e) =>
        wrapFractionInOld = false
        currentMethodCallFractionVariable = Some(k)
        try {
          val precondition = contractTranslator.translateExpression(e)
          mf.addPrecondition(spec,precondition)
        } finally {
          currentMethodCallFractionVariable = None
        }
      case chalice.Postcondition(e) =>
        wrapFractionInOld = true
        currentMethodCallFractionVariable = Some(k)
        try {
          val postcondition = contractTranslator.translateExpression(e)
          mf.addPostcondition(spec,postcondition)
        }finally{
          currentMethodCallFractionVariable = None
          wrapFractionInOld = false
        }
      case otherSpec => report(messages.UnknownAstNode(otherSpec))
    })

    methodFactory.finalizeSignature()

    k
  }
  
  val readFractionVariable = createSignature();

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////      CONTROL FLOW GRAPH SKETCH / TRANSLATION TO SSA FORM             /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Executes `body` with the supplied [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.ChaliceBlock]] and
    * [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.AssignmentInterpretation]] in scope. Ensures that the original
    * state is restored, even in the case of `body` failing with an exception.
    * @param chaliceBlock The Chalice block to refer to.
    * @param assignmentInterpretation Optional. By default [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.AssignmentInterpretation.atBeginning]] is used.
    * @param body The expression to evaluate with `chaliceBlock` in scope.
    * @tparam T The type of the body's return value.
    * @return The value returned by the body.
    */
  protected def withChaliceBlock[T](chaliceBlock : ChaliceBlock, assignmentInterpretation : AssignmentInterpretation = null)(body : => T) = {
    currentChaliceBlock = chaliceBlock
    currentAssignmentInterpretation =
      if(assignmentInterpretation != null)
        assignmentInterpretation
      else
        AssignmentInterpretation.atBeginning(chaliceBlock)

    try {
      body
    } finally {
      currentChaliceBlock = null
      currentAssignmentInterpretation = null
    }
  }

  def translate(){   
    // SILAST requires the first and last block to be created separately.
    //  The first block can be used and is supplied as the block to translate into
    //  The last block, however, cannot be anticipated. We just add an edge from the
    //    exit block of the actual (translated) body to the last block.
    var firstBlock : BasicBlockFactory = null
    var lastCodeBlock : BasicBlockFactory = null;

    // Iterate over all Chalice blocks  and translate them into SIL blocks
    for(chaliceBlock <- cfg.reversePostorder){
      val loc = chaliceBlock.sourceLocation

      // Create the first SIL block for this Chalice block. Use addFirstBasicBlock when appropriate
      val beginBlock = if (chaliceBlock == cfg.entryBlock) {
        val block = implementationFactory.addFirstBasicBlock(chaliceBlock.sourceLocation,chaliceBlock.name)
        inheritChaliceBlockProperties(chaliceBlock,block)
        firstBlock = block
        block
      } else {
        addBasicBlock(chaliceBlock)
      }
      
      chaliceBlock.silBeginBlock = beginBlock

      withChaliceBlock(chaliceBlock){
        val (_,exitBody) = into(beginBlock,{
          // Generate inhale statements for all the ϕ-assignments
          for {
            v <- chaliceBlock.assignedVariables
            vi = chaliceBlock.blockVariableInfo(v)
            if vi.needsΦAssignment
            if chaliceBlock.versionsInScope contains vi.firstVersion //Do not generate ϕ-assignments if the target version is never used
          } {
            // create ϕ assignment:
            // currently implemented as `inhale (v_1 = v_a ∨ v_1 = v_b ∨ ... ∨ v_1 = v_z)`
            val tv = vi.firstVersion //the target variable (of the ϕ assignment)
            val fac = currentExpressionFactory
            (vi.ϕ.toStream :+ tv).foreach{ v =>
              assert(currentBlock.programVariables contains programVariables(v),
                "SIL block %s (backing chalice block %s) is expected to have version %s in scope."
                .format(currentBlock.name,chaliceBlock,v))
            }
            val ϕAssignment = vi.ϕ
              .map(sv => fac.makeEqualityExpression(loc, // create expression `tv = sv`
              fac.makeProgramVariableTerm(loc,programVariables(tv)),
              fac.makeProgramVariableTerm(loc,programVariables(sv)))) //sv is the source variable of the ϕ assignment
              .reduce[Expression](fac.makeBinaryExpression(loc,Or()(chaliceBlock.sourceLocation),_,_)) // connect via logical or
            currentBlock.appendInhale(loc,ϕAssignment)
          }

          // finally, translate the statements in this Chalice block. Might result in additional SIL blocks being created
          translateStatements(new MethodCodeTranslator,chaliceBlock.statements)
        })

        // Assign the method exit block
        if(chaliceBlock == cfg.exitBlock){
          lastCodeBlock = exitBody
        }

        chaliceBlock.silEndBlock = exitBody

        //Note: we cannot implement the control flow of the Chalice CFG here, because some of the SIL end-blocks
        //  have not yet been created.
      }
    }

    assert(firstBlock != null, "firstBlock was not created.")

    // Since we don't know in advance how many SIL blocks a call to translate(*) is going to result in,
    //  we have to create the SIL exit block separately and then link from the `lastCodeBlock` to the SIL exit block
    val lastBlock = implementationFactory.addLastBasicBlock(method,getNextName("exit_body"))
    basicBlocks.addExternal(lastBlock) //TODO: assign out variables from SSA form
    lastCodeBlock.addSuccessor(method,lastBlock,TrueExpression()(method),false)

    // Assign out parameters from the respective last versions in the
    for {
      outParam <- method.outs
      outVi = cfg.exitBlock.blockVariableInfo(outParam)
      tv = programVariables.lookup(outParam.UniqueName)
      sv = programVariables(outVi.lastVersion)
    }{
      lastBlock.addProgramVariableToScope(sv)
      lastBlock.appendAssignment(method,tv,lastBlock.makeProgramVariableTerm(method,sv))
    }

    val guardTranslator = new MethodCodeTranslator
    // Finally, implement the Chalice CFG by looping over all Chalice blocks and adding
    //  the translated edges to the `silEndBlock` of each Chalice block.
    for {
      chaliceBlock <- cfg.reversePostorder
    } {

      /**
        * Ensures that the `body` is executed in a context where not just `chaliceBlock` but also
        * the corresponding `silEndBlock` are in scope together with an [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.AssignmentInterpretation.atEnd]] interpretation.
        * The body is not allowed to create additional blocks.
        * @param body
        * @tparam T
        * @return
        */
      def withEndBlock[T](body : => T) = {
        val (v,newEndBlock) = withChaliceBlock(chaliceBlock,AssignmentInterpretation.atEnd(chaliceBlock)){
          val block = chaliceBlock.silEndBlock
          blockStack.push(block)
          try{
            (body,currentBlock)
          }finally{
            blockStack.pop()
          }
        }
        
        assert(newEndBlock == chaliceBlock.silEndBlock)
        v
      } 

      def translateCondition(cs : Seq[chalice.Expression]) : Expression = withEndBlock {
        val ts = cs map guardTranslator.translateExpression
        if(ts.isEmpty)
          TrueExpression()(noLocation)
        else
          ts.reduce(currentExpressionFactory.makeBinaryExpression(chaliceBlock.sourceLocation,And()(chaliceBlock.sourceLocation),_,_))
      }

      /**
        * Implements the supplied [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.ChaliceEdge]], using
        * an already translated `condition`. Note that the condition is expected to be positive, i.e.,
        * a SIL-level negation will be added for edges with [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.ChaliceEdge.isInverted]] set to `true`.
        * @param edge
        * @param condition
        */
      def addEdge(edge : ChaliceEdge,  condition : Expression){
        val finalCondition =
          if(edge.isInverted)
            withEndBlock(currentExpressionFactory.makeUnaryExpression(chaliceBlock.sourceLocation,Not()(chaliceBlock.sourceLocation),condition))
          else
            condition
        
        edge.origin.silEndBlock.addSuccessor(
          chaliceBlock.sourceLocation, 
          edge.destination.silBeginBlock,
          finalCondition,
          edge.isBackEdge)
      }

      // We don't blindly iterate over the successor set, because we want to translate edge pairs
      //   of the form `(c ⇒ a, ¬c ⇒ b)` specially.
      var successors = chaliceBlock.successors.toSet
      while(!successors.isEmpty){
        val someEdge = successors.head
        successors -= someEdge
        val someCondition = translateCondition(someEdge.condition)
        successors.find({
          case ChaliceEdge(_,dest,cond,isInverted,_) =>
            dest == someEdge.destination &&
            cond == someEdge.condition &&
            isInverted == !someEdge.isInverted
          case _ => false
        }) match {
          case Some(otherEdge) =>
            successors -= otherEdge
            //We have found an edge pair

            val posEdge = if(someEdge.isInverted) otherEdge else someEdge
            val negEdge = if(someEdge.isInverted) someEdge else otherEdge
            
            addEdge(posEdge, someCondition)
            addEdge(negEdge, someCondition)
          case None =>
            // A single edge
            addEdge(someEdge, someCondition)
        }
      }
    }
    
    assert(cfg.entryBlock.silBeginBlock != null)
  }

  def translateAssignment(codeTranslator : CodeTranslator, lhs : chalice.VariableExpr,  rhs : chalice.RValue){
    // Watch out: It is important that `rhs` is translated *before* the assignment to `lhs` is registered
    val rhsTerm = translatePTerm(codeTranslator, rhs)

    val targetVersion = programVariables(currentAssignmentInterpretation.registerAssignment(lhs.v))
    assert(currentBlock.programVariables contains targetVersion,
      "The SIL basic block %s is expected to have the SIL program variable %s in scope. Program variables actually in scope: {%s}"
        .format(currentBlock.name,targetVersion,currentBlock.programVariables.mkString(", ")))
    currentBlock.appendAssignment(lhs,targetVersion,rhsTerm)
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
    
    blockStack.pop()
    blockStack.push(block)
  }
  
  protected def hasOutgoingEdges(block : BasicBlockFactory) : Boolean = !block.compile().successors.isEmpty

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////      TRANSLATE STATEMENTS                                            /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class MethodCodeTranslator extends DefaultCodeTranslator(thisMethodTranslator) {
    override protected def readFraction(location : SourceLocation) =
      currentExpressionFactory.makeProgramVariableTerm(location, readFractionVariable)
  }

  type CodeTranslator = ExpressionTranslator with TermTranslator with PermissionTranslator

  def translateStatements(codeTranslator : CodeTranslator, stmts : Traversable[chalice.Statement]){
    stmts.foreach(translateStatement(codeTranslator, _))
  }

  def translateStatement(codeTranslator : CodeTranslator, stmt : chalice.Statement) {
    require(!blockStack.isEmpty); 
    val oldStackSize = blockStack.length
    val stackTail = blockStack.tail.toSeq
    
    stmt match {
      case chalice.BlockStmt(body) => translateStatements(codeTranslator, body)
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
        def assignViaVar(rcvrVar : ProgramVariable){
          currentBlock.appendFieldAssignment(stmt,rcvrVar,fields(location.f),translatePTerm(codeTranslator, rhs))
        }
        location.e match {
          case rcvr:chalice.VariableExpr => assignViaVar(localVariableVersion(rcvr.v))
          case chalice.ImplicitThisExpr() => assignViaVar(methodFactory.thisVar)
          case chalice.ExplicitThisExpr() => assignViaVar(methodFactory.thisVar)
          case rcvr =>
            temporaries.using(referenceType){ rcvrVar =>
              currentBlock.appendAssignment(rcvr,rcvrVar,translatePTerm(codeTranslator, rcvr))
              currentBlock.appendFieldAssignment(stmt,rcvrVar,fields(location.f),translatePTerm(codeTranslator, rhs))
            }
        }
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
  protected final case class ReadImplication(lhs : Expression, rhs : List[ReadCondition])
    extends ReadCondition
    with Product2[Expression, List[ReadCondition]] {
    def _1 = lhs
    def _2 = rhs
  }
  protected final case class ReadField(reference : Term, field : FieldTranslator) extends ReadCondition
  /**
    * Translates a single Chalice statement by appending SIL statements to the current block
    * and/or creates edges to new blocks. The number of blocks on the `blockStack` is expected
    * to remain the same, but the top element might change.
    */
  def translateMethodCall(callNode : chalice.Call) {
    val codeTranslator = new MethodCodeTranslator
    val chalice.Call(_,destinations,receiver,_,args) = callNode
    val calleeFactory = methods(callNode.m.asInstanceOf[chalice.Method])

    //Read (fractional) permissions
    val readFractionVar = allocateLocallyScoped(callNode, "k", permissionType) // we won't give it back, ever
    val readFractionTerm = currentExpressionFactory.makeProgramVariableTerm(callNode, readFractionVar)
    // `inhale 0 < k`
    currentBlock.appendInhale(callNode,currentExpressionFactory.makeDomainPredicateExpression(callNode,
      permissionLT,TermSequence(currentExpressionFactory.makeNoPermission(callNode),readFractionTerm)))
    
    // Permission maps
    // `var m_0 : Map[(ref,int),Permission]`
    val originalPermMapVar = allocateLocallyScoped(callNode,"m0",prelude.Map.PermissionMap.dataType)
    val originalPermMapTerm = currentExpressionFactory.makeProgramVariableTerm(callNode,originalPermMapVar)
    // `var m : Map[(ref,int),Permission]`
    val permMapVar = allocateLocallyScoped(callNode,"m",prelude.Map.PermissionMap.dataType)
    val permMapTerm = currentExpressionFactory.makeProgramVariableTerm(callNode,permMapVar)
    // `inhale m = m_0`
    currentBlock.appendInhale(callNode,currentExpressionFactory.makeEqualityExpression(callNode,
      permMapTerm,originalPermMapTerm
    ))

    // Translate arguments and create mapping from parameter variables to these terms
    val argTerms =
      currentExpressionFactory.makeProgramVariableTerm (callNode,thisVariable) ::
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
    val moveToBlock = new MoveToBlock(this)

    // Generate assumptions and conditions on fraction
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
        List(ReadImplication(transplantExpression(lhs),genReadCond(rhs)))
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

    def appendCond(rs : List[ReadCondition]){
      rs foreach { 
        case ReadField(reference,field) =>
          val localReference = moveToBlock.transplant(reference)
          val originalPermMapTerm = currentExpressionFactory.makeProgramVariableTerm(callNode,originalPermMapVar)
          val permMapTerm = currentExpressionFactory.makeProgramVariableTerm(callNode,permMapVar)
          val readFractionTerm = currentExpressionFactory.makeProgramVariableTerm(callNode,readFractionVar)

          val currentActualPermission = currentExpressionFactory.makePermTerm(callNode,localReference,field)
          temporaries.using(prelude.Pair.Location.dataType){ locationVar =>
            val location = currentExpressionFactory.makeProgramVariableTerm(callNode,locationVar)

            // `location := (ref,field)`
            currentBlock.appendAssignment(callNode,locationVar,
              field.locationLiteral(currentExpressionFactory, localReference.asInstanceOf[PTerm]))

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

    calleeFactory.method.signature.precondition
      .map(genReadCond _)
      .foreach(appendCond _)

    // Generate call statement
    val receiverTerm = translatePTerm(codeTranslator,receiver)
    val destinationVars = destinations.map(vExpr =>
      programVariables(currentAssignmentInterpretation.registerAssignment(vExpr.v)))
    currentBlock.appendCall(
      callNode,
      currentBlock.makeProgramVariableSequence(callNode, destinationVars),
      receiverTerm,
      calleeFactory,
      PTermSequence(argTerms.map(t => moveToBlock.transplant(t).asInstanceOf[PTerm]) : _*))
  }

  def translateAssert(expr : chalice.Expression) {
    val translator = new MethodCodeTranslator with AssertionTranslator
    currentBlock.appendExhale(expr,translator.translateExpression(expr))
  }

  def translateAssume(expr : chalice.Expression) {
    val translator = new MethodCodeTranslator with AssertionTranslator
    currentBlock.appendInhale(expr,translator.translateExpression(expr))
  }
  
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////      TRANSLATE TERMS                                                 /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def translatePTerm(codeTranslator : CodeTranslator, rvalue : chalice.RValue) : PTerm = {
    codeTranslator.translateTerm(rvalue) match {
      case pt:PTerm => pt
      case t =>
        assert(false,"Expected program term. Actual type: %s. Location: %s.".format(t.getClass,t.sourceLocation))
        null
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////      METHOD CALL FRACTION VARIABLE                                   /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  var wrapFractionInOld : Boolean = false
  var currentMethodCallFractionVariable : Option[ProgramVariable] = None
  def makeCurrentMethodCallFraction(sourceLocation : SourceLocation) : Term = {
    require(currentMethodCallFractionVariable.isDefined,"No method call site-specific k defined at this location.")
    val varTerm = currentExpressionFactory.makeProgramVariableTerm(sourceLocation,currentMethodCallFractionVariable.get)
    if(wrapFractionInOld){
      currentExpressionFactory.makeOldTerm(sourceLocation,varTerm)
    }else{
      varTerm
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////      HELPER FUNCTIONS (TRANSLATION DSL)                              /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected def silIf[T](cond : Expression, conditionLocation : SourceLocation = noLocation)(thnBlock : => T) = new {
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
                                  condExpr : Expression,
                                  thn : () => T,
                                  elsOpt : Option[() => U] = None,
                                  thnLoc : Option[SourceLocation] = None,
                                  elsLoc : Option[SourceLocation] = None) : (T, Option[U]) = {

    val thenLocation = thnLoc.getOrElse(noLocation)
    val elseLocation = elsLoc.getOrElse(noLocation)

    //Create block for then-branch. (else is only created when necessary)
    val thenBlock = addBasicBlock(currentChaliceBlock, "if_then")

    //Compile then-branch
    //  first, connect to current block via condition

    currentBlock.addSuccessor(thenLocation,thenBlock,condExpr,false)
    //  then, compile body of then-branch
    val (thenResult,endThenBlock) = into(thenBlock,thn())

    // IMPORTANT: do not create the nextBlock until both branches are compiled
    //  otherwise, temporary variables declared in else might not be in scope!
    lazy val nextBlock = addBasicBlock(currentChaliceBlock, "if_continue")

    //Handle else-block if there is one. "elseSuccessor" is the block the control should be transferred to
    //  when the condition is false. This is either the actual successor block or the else-branch

    val (elseResult,elseSuccessor) = elsOpt match {
      case Some(els) =>
        //Compile else-branch, same as then-branch
        val elseBlock = addBasicBlock(currentChaliceBlock, "if_else")
        val (result,elseBlockEnd) = into(elseBlock,els())

        elseBlockEnd.addSuccessor(elseLocation,nextBlock,TrueExpression()(elseLocation),false)
        (Some(result),elseBlock)
      case None => (None,nextBlock)
    }

    //  finally, connect then-block to successor with no condition (True)
    endThenBlock.addSuccessor(thenLocation,nextBlock,TrueExpression()(thenLocation),false)

    //Create control transfer in case the condition does not hold ↔ ¬condition holds
    currentBlock.addSuccessor(noLocation,elseSuccessor,currentBlock.makeUnaryExpression(
      elseLocation,Not()(elseLocation),condExpr),false)

    //Update currentBlock
    this.continueWith(nextBlock)

    (thenResult,elseResult)
  }

}