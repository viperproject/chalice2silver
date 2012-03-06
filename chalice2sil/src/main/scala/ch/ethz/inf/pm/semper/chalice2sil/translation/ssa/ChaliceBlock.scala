package ch.ethz.inf.pm.semper.chalice2sil.translation.ssa
 
import collection.generic.Growable
import collection.mutable.Buffer
import collection._
import ch.ethz.inf.pm.semper.chalice2sil._
import silAST.source.{SourceLocation, noLocation}
import silAST.methods.implementations.BasicBlockFactory
import silAST.programs.symbols.ProgramVariable

/**
  * A chalice-level basic block with links to predecessors and successors. Used to
  * translate chalice variable assignments to SSA form.
  * @param name The unique name (per method implementation) for this block.
  */
class ChaliceBlock(val name : String) { origin =>
  require(name != null)

  /**
    * The source location associated with this block. When available, this is the location
    * of the first statement in the block.
    */
  def sourceLocation : SourceLocation = statements.headOption.map(astNodeToSourceLocation).getOrElse(noLocation)

  /**
    * The sequence of (basic) statements for this node. Might still contain expressions that have to be translated into
    * more control flow (IfThenElse expressions, for instance)
    */
  val statements = mutable.Buffer[chalice.Statement]()

  /**
    * The set of outgoing control flow edges. Usually either one unconditional edge or two conditional edges.
    * Usually the lack of outgoing edges indicates that this block is the exit block.
    */
  val successors = mutable.Set[ChaliceEdge]()

  /**
    * The set of outgoing control flow edges.
    * Usually the lack of outgoing edges indicates that this block is the exit block.
    */
  val predecessors = mutable.Set[ChaliceEdge]()

  /**
    * The list of loop invariants to be checked when control passes this block. While-loop heads are blocks without any
    * statements but two outgoing edges and a list of invariants.
    */
  val invariants = mutable.Buffer[chalice.Expression]()
  
  private def conditionalTransfer(cond : List[chalice.Expression], isInverted : Boolean) = new {
    def -->(target : ChaliceBlock) {
      addSuccessor(ChaliceEdge(origin,target,cond,isInverted))
    }
    def backedgeTo(target : ChaliceBlock) {
      addSuccessor(ChaliceEdge(origin,target,cond,isInverted,isBackEdge = true))
    }
  }
  
  def ?(cond : chalice.Expression) = conditionalTransfer(List(cond), false)
  def ?∧(cond : List[chalice.Expression]) = conditionalTransfer(cond,false)
  def ?¬(cond : chalice.Expression) = conditionalTransfer(List(cond), true)
  def -->(target : ChaliceBlock) {
    addSuccessor(ChaliceEdge(origin,target))
  }
  def backedgeTo(target : ChaliceBlock) {
    addSuccessor(ChaliceEdge(origin,target,isBackEdge = true))
  }
  
  private def addSuccessor(edge : ChaliceEdge){
    require(edge.origin == origin)
    require(origin.successors.forall(_.destination != edge.destination),
      "Chalice block %s already links to block %s.".format(origin,edge.destination))
    
    successors += edge
    edge.destination.predecessors += edge
  }

  override def hashCode() = name.hashCode()

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////      ANNOTATIONS FOR SSA FORM TRANSLATION                                         /////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val assignedVariables = mutable.Set[chalice.Variable]()
  
  var dominators : immutable.Set[ChaliceBlock] = null
  private[ssa] var _immediateDominator : ChaliceBlock  = null

  def immediateDominator = {
    require(_immediateDominator != null,"Tried to access the immediate dominator of the entry node.")
    _immediateDominator
  }
  
  val dominanceFrontier = mutable.Set[ChaliceBlock]()
  
  val blockVariableInfoMap = mutable.Map[chalice.Variable,BlockVariableInfo]()  
  def blockVariableInfo(v : chalice.Variable) = blockVariableInfoMap.getOrElseUpdate(v,new BlockVariableInfo(this,v))

  /**
    * Adds an argument (a block) to the ϕ assignment for the specified variable.
    *
    * At this stage, instead of directly mentioning the variable version in the argument list of the
    * ϕ assignment, we just refer to the block that will supply the version. This is necessary, because
    * the variable version numbers will only be assigned afterwards, during the translation to the SIL AST.
    * @param v The variable for which to add an argument to the ϕ assignment.
    * @param block The block to add to the ϕ-assignment.
    * @return True if the block was not yet part of the ϕ assignment; false otherwise.
    */
  def addΦEntry(v : chalice.Variable, block : ChaliceBlock) : Boolean ={
    require(block.assignedVariables contains v)
    val bvi = blockVariableInfo(v)
    val old = bvi.needsΦAssignment
    bvi.needsΦAssignment = true
    assignedVariables += v

    !old // return true iff needsΦAssignment was false before
  }
  
  var versionsInScope : immutable.Set[Version] = null
  var temporariesInScope : immutable.Set[ProgramVariable] = immutable.Set[ProgramVariable]()
  
  var initializedVariables : immutable.Set[chalice.Variable] = immutable.Set()

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////      ANNOTATIONS CHALICE TO SIL TRANSLATION                                       /////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  private[this] var _silBeginBlock : BasicBlockFactory = null
  private[this] var _silEndBlock   : BasicBlockFactory = null
  
  def silBeginBlock = {
    require(_silBeginBlock != null, "The underlying SIL begin block has not yet been assigned to chalice block %s.".format(this))
    _silBeginBlock
  }
  
  def silBeginBlock_=(beginBlock : BasicBlockFactory){
    _silBeginBlock = beginBlock
  }

  def silEndBlock_=(endBlock : BasicBlockFactory){
    _silEndBlock = endBlock
  }

  def silEndBlock = {
    require(_silEndBlock != null, "The underlying SIL end block has not yet been assigned to chalice block %s.".format(this))
    _silEndBlock
  }
  
  override def toString = name
}
