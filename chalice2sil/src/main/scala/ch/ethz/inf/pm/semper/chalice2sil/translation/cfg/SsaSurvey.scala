package ch.ethz.inf.pm.semper.chalice2sil.translation.cfg

import ch.ethz.inf.pm.semper.chalice2sil._
import translation.{NameSequence, MethodEnvironment, ProgramEnvironment, DerivedProgramEnvironment}
import collection._


/**
  * Extracts chalice basic blocks [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.ChaliceBlock]] from a
  * chalice method, while also translating chalice local variable assignments into SSA form.
  * The result is a control flow graph with a hybrid of chalice statements and SSA assignments.
  * @author Christian Klauser
  */
class SsaSurvey(programEnvironment: ProgramEnvironment)
  extends DerivedProgramEnvironment(programEnvironment) {

  private[this] val nameSequence = NameSequence()

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
    * Creates a new [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.ChaliceBlock]] with a unique name, optionally
    * prefixed with the supplied string.
    * @param prefix The prefix of the generated block name. Optional.
    * @return The created empty block.
    */
  protected def createBlock(prefix : String = "") = new ChaliceBlock(getNextName(prefix))
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////  TRANSLATE TO CONTROL FLOW GRAPH                                                  ///////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def translateControlFlow(method : chalice.Method) = {
    val entryBlock = createBlock("entry")
    val exitBlock =  translateStatementList(entryBlock,method.body)
    new ControlFlowSketch(entryBlock, exitBlock)
  }

  /**
    * Translates a sequence of statements. Starts by appending to the supplied
    * [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.ChaliceBlock]] and then switched to whatever blocks are
    * returned by [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.SsaSurvey.translateStatement]].
    * @param head The block to start appending chalice statements to.
    * @param stmts The sequence of chalice statements to translate to extract basic blocks from.
    * @return The chalice block that the last statement ended in. This block does not yet have outgoing edges;
    * you are free to append more statements to it.
    */
  protected def translateStatementList(head : ChaliceBlock, stmts : Seq[chalice.Statement]) : ChaliceBlock =
    stmts.foldLeft(head)(translateStatement(_,_)).ensuring(_.successors.isEmpty)

  /**
    * Translates a single chalice statement into the CFG sketch. Statements that do not represent control flow,
    * are appended to the supplied [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.ChaliceBlock]], while
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
      val thnBlock = createBlock("then")
      val endThnBlock = translateStatement(thnBlock, thn)
      val endIfBlock = createBlock("endif")

      elsOpt match {
        case None =>
          //connect blocks
          head ? cond --> thnBlock
          head ?¬cond --> endIfBlock
          endThnBlock --> endIfBlock
        case Some(els) =>
          val elsBlock = createBlock("else")
          val endElsBlock = translateStatement(elsBlock, stmt)

          //connect blocks
          head ? cond --> thnBlock
          head ?¬cond --> elsBlock
          endThnBlock --> endIfBlock
          endElsBlock --> endIfBlock
      }

      endIfBlock
    case w@chalice.WhileStmt(cond,_,_,_,body) =>
      val whileContinue = createBlock("while_continue")
      val beginBody = createBlock("while_begin")
      val endBody = translateStatement(beginBody, body)
      val endWhile = createBlock("while_end")

      head --> whileContinue
      whileContinue ? cond --> beginBody
      whileContinue ?¬cond --> endWhile
      endBody backedgeTo whileContinue

      endWhile
    case chalice.Assign(vExpr,_) =>
      head.assignedVariables += vExpr.v
      head
    case chalice.LocalVar(v,Some(_)) => 
      head.assignedVariables += v
      head
    case chalice.Call(_,lhsExprs,_,_,_) =>
      lhsExprs foreach { vExpr =>
        head.assignedVariables += vExpr.v
      }
      head
    case chalice.Receive(_,_,lhsExprs) => 
      lhsExprs foreach { vExpr =>
        head.assignedVariables += vExpr.v
      }
      head
    case chalice.CallAsync(_,vExpr,_,_,_) =>
      head.assignedVariables += vExpr.v
      head
    case chalice.JoinAsync(lhsExprs,_) =>
      lhsExprs foreach { vExpr =>
        head.assignedVariables += vExpr.v
      }
      head
    case _ =>
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

  def determineΦ(cfg : ControlFlowSketch) {
    val workSet = mutable.Set[ChaliceBlock](cfg.reversePostorder.filterNot(_.assignedVariables.isEmpty):_*)
    while(!workSet.isEmpty){
      val b = workSet.head
      workSet -= b

      for(f <- b.dominanceFrontier){
        var changed = false;
        for(v <- b.assignedVariables){
          changed ||= f.addΦEntry(v,b)
        }
        if(changed)
          workSet += f
      }
    }
  }
}
