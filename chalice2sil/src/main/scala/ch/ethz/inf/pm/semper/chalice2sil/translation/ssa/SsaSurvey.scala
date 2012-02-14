package ch.ethz.inf.pm.semper.chalice2sil.translation.ssa

import ch.ethz.inf.pm.semper.chalice2sil._
import collection._
import translation._
import ssa._
import silAST.programs.symbols.ProgramVariable


/**
  * Extracts chalice basic blocks [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.ChaliceBlock]] from a
  * chalice method, while also translating chalice local variable assignments into SSA form.
  * The result is a control flow graph with a hybrid of chalice statements and SSA assignments.
  * @author Christian Klauser
  */
class SsaSurvey(programEnvironment: ProgramEnvironment, nameSequence : NameSequence)
  extends DerivedProgramEnvironment(programEnvironment) {

  /**
    * Generates a new unique name, optionally prefixed with the supplied string to aid debugging.
    * @param prefix The prefix of the generated name. Optional.
    * @return a unique name that is guaranteed to start with the supplied prefix.
    */
  protected def getNextName(prefix : String = "") =
    if(prefix == "")
      nameSequence.nextName
    else
      prefix + "_" + nameSequence.nextName

  /**
    * Creates a new [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.ChaliceBlock]] with a unique name, optionally
    * prefixed with the supplied string.
    * @param prefix The prefix of the generated block name. Optional.
    * @return The created empty block.
    */
  protected def createBlock(prefix : String = "") = createNamedBlock(getNextName(prefix))
  protected def createNamedBlock(name : String) = new ChaliceBlock(name)
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////  TRANSLATE TO CONTROL FLOW GRAPH                                                  ///////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def translateControlFlow(method : chalice.Method) = {
    val entryBlock = createBlock("entry")
    val exitBlock =  translateStatementList(entryBlock,method.body)
    val cfg = new ControlFlowSketch(entryBlock, exitBlock, method.ins, method.outs)
    
    // Treat entry block as the one "assigning" parameters (both in and out)
    val parameters = (cfg.ins.toStream ++ cfg.outs)
    entryBlock.assignedVariables ++= parameters

    //cfg.localVariables is defined in terms of assignedVariables, so in order to get ins and outs included, we need to
    // add them separately
    entryBlock.assignedVariables ++= cfg.localVariables
    entryBlock.initializedVariables ++= cfg.localVariables

    cfg
  }

  /**
    * Translates a sequence of statements. Starts by appending to the supplied
    * [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.ChaliceBlock]] and then switched to whatever blocks are
    * returned by [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.SsaSurvey.translateStatement]].
    * @param head The block to start appending chalice statements to.
    * @param stmts The sequence of chalice statements to translate to extract basic blocks from.
    * @return The chalice block that the last statement ended in. This block does not yet have outgoing edges;
    * you are free to append more statements to it.
    */
  protected def translateStatementList(head : ChaliceBlock, stmts : Seq[chalice.Statement]) : ChaliceBlock =
    stmts.foldLeft(head)(translateStatement(_,_)).ensuring(_.successors.isEmpty)

  /**
    * Translates a single chalice statement into the CFG sketch. Statements that do not represent control flow,
    * are appended to the supplied [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.ChaliceBlock]], while
    * statements like `if` and `while` are translated into the corresponding block sub-graphs.
    * @param head The block to append the statement to or from which to branch away.
    * @param stmt The statement to be translated.
    * @return the block that control flow ends up in after the statement is executed. This block is guaranteed not to
    * have any outgoing edges yet. You are therefore free to append additional statements to it.
    */
  protected def translateStatement(head : ChaliceBlock, stmt : chalice.Statement) : ChaliceBlock = stmt match {
    case chalice.BlockStmt(stmts) => 
      translateStatementList(head, stmts)
    case chalice.IfStmt(cond, thn, elsOpt) =>
      val instanceName = getNextName("if")
      val thnBlock = createNamedBlock(instanceName + "_then")
      val endThnBlock = translateStatement(thnBlock, thn)
      val endIfBlock = createNamedBlock(instanceName + "_endif")

      elsOpt match {
        case None =>
          //connect blocks
          head ? cond --> thnBlock
          head ?¬cond --> endIfBlock
          endThnBlock --> endIfBlock
        case Some(els) =>
          val elsBlock = createNamedBlock(instanceName + "_else")
          val endElsBlock = translateStatement(elsBlock, els)

          //connect blocks
          head ? cond --> thnBlock
          head ?¬cond --> elsBlock
          endThnBlock --> endIfBlock
          endElsBlock --> endIfBlock
      }

      endIfBlock
    case w@chalice.WhileStmt(cond,_,_,_,body) =>
      val instanceName = getNextName("while")
      val whileContinue = createNamedBlock(instanceName + "_continue")
      val beginBody = createNamedBlock(instanceName + "_begin")
      val endBody = translateStatement(beginBody, body)
      val endWhile = createNamedBlock(instanceName + "_end")

      head --> whileContinue
      whileContinue ? cond --> beginBody
      whileContinue ?¬cond --> endWhile
      endBody backedgeTo whileContinue

      endWhile
    case _ =>
      head.assignedVariables ++= stmt.assignedVariables
      head.statements += stmt
      head
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////  DETERMINE DOMINANCE FRONTIERS                                                    ///////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def determineDominators(cfg : ControlFlowSketch){
    /**
      * Indexed by postorder number of blocks. Each entry is either None
      * or Some(postorder number of immediate dominator). Initially, no 
      * immediate dominators are known. Unknown values are marked with a -1.
      */
    val doms = Array.fill(cfg.postorder.size)(-1)
    
    def ρ(block : ChaliceBlock) = cfg.postorderNumbers(block)
    
    // initiate algorithm by setting the entry block to be its own dominator
    doms(ρ(cfg.entryBlock)) = ρ(cfg.entryBlock)
    
    var changed = true
    val iterNodes = cfg.reversePostorder.tail // we can safely skip the entry node. Its dominator is trivially just itself.
    /**
      * Find a dominator that blocks `b1` and `b2` have in common. In particular, select the "closest" such dominator
      * in order to get an immediate dominator.
      * @param b1 The postorder number of block 1
      * @param b2 The postorder number of block 2
      * @return The postorder number of the common immediate dominator
      */
    def intersect(b1 : Int, b2 : Int) = {
      var finger1 = b1
      var finger2 = b2
      while(finger1 != finger2){
        while(finger1 < finger2)
          finger1 = doms(finger1)
          assert(0 <= finger1)
        while(finger2 < finger1)
          finger2 = doms(finger2)
      }
      finger1
    }
    while(changed){
      changed = false
      for(b <- iterNodes){
        val newIdom = b.predecessors
          .map(e => ρ(e.origin))
          .filter(e => 0 <= doms(e)) //only consider predecessors that have already been processes, i.e. doms(p) != -1
          .reduceLeft(intersect)     // --> this makes the algorithm a fixed-point algorithm. We have to iterate
        val ρb = ρ(b)                // until we have found a "steady state"
        if (doms(ρb) != newIdom) {
          doms.update(ρb,newIdom)
          changed = true
        }
      } 
    }
    
    // Update control flow graph with information in `doms`
    val domCache = mutable.Map[Int,List[Int]]()
    def dominators(block : Int) : List[Int] = {
      val idom = doms(block)
      if(idom == block)
        List(block)
      else
        block :: domCache.getOrElseUpdate(idom,dominators(idom))
    }
    
    for(b <- iterNodes){
      assert(b != cfg.entryBlock)
      val ρb = ρ(b)
      b._immediateDominator = cfg.postorder(doms(ρb))
      b.dominators = dominators(ρb).map(cfg.postorder).toSet
    }
    
    cfg.entryBlock.dominators = immutable.Set(cfg.entryBlock)
  }

  def determineDominanceFrontiers(cfg : ControlFlowSketch){
    /* Taken from "A Simple, Fast Dominance Algorithm;
        Keith D. Cooper, Timothy J. Harvey and Ken Kennedy; Software - Practice and Experience 2001; 4:1-10"
     */
    if(cfg.entryBlock == cfg.exitBlock)
      return; //nothing to do, CFG is trivial
    require(cfg.exitBlock._immediateDominator != null,"Dominators not yet determined on control flow graph.")

    def doms(block : ChaliceBlock) = 
      if (block == cfg.entryBlock)
        block
      else
        block.immediateDominator
    
    // only consider "join-point" where different code paths converge
    for(b <- cfg.reversePostorder){
      if(b.predecessors.size > 1){
        for (e <- b.predecessors){
          var runner = e.origin
          while(runner != b.immediateDominator){
            runner.dominanceFrontier += b
            runner = doms(runner)
          }
        }
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////  DETERMINE LOCATION OF ϕ ASSIGNMENTS                                              ///////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def determineΦLocations(cfg : ControlFlowSketch) {
    val workSet = mutable.Set[ChaliceBlock](cfg.reversePostorder.filterNot(_.assignedVariables.isEmpty):_*)
    while(!workSet.isEmpty){
      val b = workSet.head
      workSet -= b

      for(f <- b.dominanceFrontier){
        var changed = false;
        for(v <- b.assignedVariables){
          changed |= f.addΦEntry(v,b)
        }
        if(changed)
          workSet += f
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////  DETERMINE VARIABLE VERSIONS                                                      ///////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def determineIntermediateVersions(cfg : ControlFlowSketch) {
    import ssa._
    import collection._

    //Determine intermediate local variable versions (excluding the ones used in ϕ assignments)
    for(block <- cfg.reversePostorder){
      def createVersion(v : chalice.Variable) =
        Version.intermediate(v, getNextName())

      val vBuilders = block.assignedVariables.map(v => v -> immutable.IndexedSeq.newBuilder[Version]).toMap

      //Add version for ϕ assignment where needed
      for(v <- block.assignedVariables){
        val vi = block.blockVariableInfo(v)
        if(vi.needsΦAssignment)
          vBuilders(v) += createVersion(v)
      }

      // Iterate over all statements and add versions for each variable assignment.
      // The versions are just stored in the sequence and are later correlated to the
      //  assignment statements by their position
      for(stmt <- block.statements;
          v <- stmt.assignedVariables){
        vBuilders(v) += createVersion(v)
      }

      // Build the sequence and store it in the block variable info structure
      for(v <- block.assignedVariables){
        val vi = block.blockVariableInfo(v)
        vi.versions = vBuilders(v).result()
      }
    }

    //Initialize variables/parameters
    for {
      block <- cfg.reversePostorder
      initVar <- block.initializedVariables
      vi = block.blockVariableInfo(initVar)
    }{
      val initVersion =
        if(cfg.ins.contains(initVar))
          ssa.Version.mapped(initVar)
        else
          ssa.Version.intermediate(initVar,getNextName("init"))
      vi.versions = vi.versions.+:(initVersion)
    }
  }

  /**
    * Ensures that there is a [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.BlockVariableInfo]] for every
    * block-variable pair with correctly initialized `firstVariable` and `lastVariable` fields.
    * @param cfg The control flow graph to operate on.
    */
  def determineDefinitionReach(cfg : ControlFlowSketch) {
    val workSet = mutable.Set[ChaliceBlock](cfg.reversePostorder:_*)
    while(!workSet.isEmpty){
      val block = workSet.head
      workSet -= block
      
      var changed = false
      for (v <- cfg.localVariables){
        val vi = block.blockVariableInfo(v)
        if((!vi.needsΦAssignment) && (!block.initializedVariables.contains(v))){
          val idomInfo = block.immediateDominator.blockVariableInfo(v)
          if(!idomInfo.versions.isEmpty){
            vi.versions.headOption match {
              case Some(h) if h == idomInfo.lastVersion => //do nothing, definition already reached this node
              case _ => 
                vi.versions = idomInfo.lastVersion +: vi.versions
                changed = true
            }
          }
        }
      }
      
      if(changed){
        workSet ++= block.successors.map(_.destination)
      }
    }
    
    assert(true)
    
    for {
      block <- cfg.reversePostorder
      v <- cfg.localVariables
      vi = block.blockVariableInfo(v)
      if vi.needsΦAssignment
    } {
      vi.ϕ = block.predecessors.toStream.map(_.origin.blockVariableInfo(v).lastVersion).toSet
    }
  }

  /**
    * Computes the Gen set for a given block, i.e. the set of variable versions used by that block.
    */
  def computeUsedVersions(block : ChaliceBlock) : immutable.Set[Version] = {
    val builder = immutable.Set.newBuilder[Version]

    //first,consider all ϕ assignments
    builder ++= block.assignedVariables
      .toStream
      .map(block.blockVariableInfo(_))
      .filter(_.needsΦAssignment)
      .map(_.ϕ) //note that the target of the ϕ-assignment has *not* been marked as used. The translator is expected
                // to skip a ϕ-assignment if the target variable is not in scope.
      .flatten

    //then, simulate the execution of the basic block to observe which versions are accessed
    val interpretation = AssignmentInterpretation.atBeginning(block)
    def inspect(expr : chalice.RValue) {
      chalice.AST.visit(expr,{
        case variableExpr@chalice.VariableExpr(_) => builder += interpretation.version(variableExpr.v)
        case _ =>
      })
    }
    def assign(v : chalice.Variable) {
      builder += interpretation.registerAssignment(v)
    }
    def assignMany(args : Seq[chalice.VariableExpr]) {
      args.foreach(x => assign(x.v))
    }
    block.statements foreach {
      case chalice.Acquire(e) => inspect(e)
      case chalice.Assert(e) => inspect(e)
      case chalice.Assign(lhs,rhs) =>
        inspect(rhs)
        assign(lhs.v)
      case chalice.Assume(e) => inspect(e)
      case b:chalice.BlockStmt => report(messages.UnknownAstNode(b))
      case chalice.Call(_,lhs,e,_,args) =>
        inspect(e)
        args foreach inspect
        assignMany(lhs)
      case chalice.CallAsync(_,varExpr,e,_,args) =>
        assign(varExpr.v)
        inspect(e)
        args foreach inspect
      case chalice.Downgrade(e) => inspect(e)
      case chalice.FieldUpdate(lhs,rhs) =>
        inspect(lhs)
        inspect(rhs)
      case chalice.Fold(e) => inspect(e)
      case chalice.Free(e) => inspect(e)
      case i:chalice.IfStmt => report(messages.UnknownAstNode(i))
      case chalice.Install(e,lower,upper) =>
        inspect(e)
        lower foreach inspect
        upper foreach inspect
      case chalice.JoinAsync(outs,e) =>
        inspect(e)
        assignMany(outs)
      case chalice.LocalVar(v,Some(rhs)) =>
        inspect(rhs)
        assign(v)
      case chalice.LocalVar(v,_) => //ignore, it's just a declaration
      case l:chalice.Lock => report(messages.UnknownAstNode(l)) //TODO: implement chalice lock statement block
      case chalice.RdAcquire(e) => inspect(e)
      case chalice.RdRelease(e) => inspect(e)
      case chalice.Receive(_,e,outs) =>
        inspect(e)
        assignMany(outs)
      case r:chalice.RefinementBlock => report(messages.UnknownAstNode(r))
      case chalice.Release(e) => inspect(e)
      case chalice.Send(e,args) =>
        inspect(e)
        args.foreach(inspect)
      case chalice.Share(e,lower,upper) =>
        inspect(e)
        lower foreach inspect
        upper foreach inspect
      case  chalice.Signal(e,_,_) => inspect(e)
      case chalice.SpecStmt(lhs,locals,pre,post) =>
        inspect(pre)
        inspect(post)
        builder ++= locals.map(interpretation.version(_))
        assignMany(lhs)
      case chalice.Unfold(e) => inspect(e)
      case chalice.Unshare(e) => inspect(e)
      case chalice.Wait(e,_) => inspect(e)
      case w:chalice.WhileStmt => report(messages.UnknownAstNode(w))

    }

    // finally also take conditions on CFG edges into account
    block.successors.toStream.map(_.condition).flatten.foreach(inspect)

    builder.result()
  }

  /**
    * Uses live variable analysis to determine which versions are in scope for which blocks (SIL requires this
    * information) Since we don't want to eliminate any assignments, we consider assignments "usages".
    * @param cfg The control flow graph to operate on.
    * @param parameters The parameters of the method represented by the CFG. These will be guaranteed to be alive on exit.
    */
  def determineVariableScopes(cfg : ControlFlowSketch, parameters : Seq[chalice.Variable]){
    /*
        Live_in(s) = Gen(s) ∪ (Live_out(s) - Kill(s))  
        Live_out(exit) = {}
        Live_out(s) = Union(p ∈ succ(s): Live_in(p))
        
        Note that our Kill sets are always empty, since in SSA form there are no re-assignments. A version of a variable
        is either used or it is not used.
     */
    
    val (liveIn, liveOut) = {
      val emptySet = cfg.preorder.toStream.map(_ -> immutable.Set.empty[Version]) 
      (mutable.Map(emptySet:_*),mutable.Map(emptySet:_*))
    }
    
    //Ensure that all parameters (ins and outs) are alive at the end (for use contracts)
    liveOut(cfg.exitBlock) = parameters.map(cfg.entryBlock.blockVariableInfo(_).firstVersion).toSet

    val workSet = mutable.Set(cfg.postorder:_*)
    while(!workSet.isEmpty){
      val block = workSet.head
      workSet -= block
      
      // 1. compute liveOut(block); except for the exit block, its liveOut is fixed.
      if(block != cfg.exitBlock){
        liveOut.update(block, block.successors.toStream.map(e => liveIn(e.destination)).flatten.toSet)
      }
      
      // 2. compute gen(block), the set of versions used by this block
      val gen = computeUsedVersions(block)
      
      // 3. compute kill(block), is trivially empty since we are operating on SSA form
      
      // 4. compute liveIn(block)
      val newIn = gen ++ liveOut(block)
      val oldIn = liveIn(block)
      if(newIn != oldIn){
        liveIn.update(block, newIn)
        block.predecessors.map(_.origin).foreach(workSet += _)
      }
    }

    // Finally, use the information from the data flow equation solution to fill in variable scopes.
    cfg.preorder.foreach(b => b.versionsInScope = liveIn(b))
  }
}
