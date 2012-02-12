package ch.ethz.inf.pm.semper.chalice2sil.translation.ssa

import scala.collection._;

/**
  * A control flow graph for a chalice method. This data structure is used
  * to cache the results of potentially expensive algorithms. It assumes that
  * the **structure** (edges) of the graph remains static while the object is in use.
  * 
  * @author Christian Klauser
  */
class ControlFlowSketch(val entryBlock : ChaliceBlock, val exitBlock : ChaliceBlock, val ins : immutable.Seq[chalice.Variable], val outs : immutable.Seq[chalice.Variable]) {
  require(entryBlock.predecessors.isEmpty)
  require(exitBlock.successors.isEmpty)

  lazy val postorder : Array[ChaliceBlock] = {
    val builder = mutable.ArrayBuilder.make[ChaliceBlock]()
    val visited = mutable.Set[ChaliceBlock]()

    def visitPostorder(block : ChaliceBlock){
      if(!visited.add(block))
        return;

      block.successors.view
        .filter(!_.isBackEdge)
        .map(_.destination)
        .foreach(visitPostorder)

      builder += block
    }

    visitPostorder(entryBlock)

    builder.result()
  }

  lazy val preorder : Array[ChaliceBlock] = {
    val builder = Array.newBuilder[ChaliceBlock]
    val visited = mutable.Set[ChaliceBlock]()

    def visitPreorder(block : ChaliceBlock){
      if(!visited.add(block))
        return;
      builder += block

      block.successors.view
        .filter(!_.isBackEdge)
        .map(_.destination)
        .foreach(visitPreorder)
    }

    visitPreorder(entryBlock)

    builder.result()
  }

  lazy val postorderNumbers : immutable.Map[ChaliceBlock,Int] = postorder.zipWithIndex.toMap

  /**
    * This is a typical iteration order for forward data-flow problems. In reverse-postorder iteration, a node is 
    * visited before all its successor nodes have been visited, except when the successor is reached by a back edge. 
    * (Note that this is not the same as preorder.)
    * @return The list of all nodes reachable from the entry node in reverse-postorder.
    */
  lazy val reversePostorder : List[ChaliceBlock] = postorder.reverseIterator.toList

  /**
    * The size of the control flow graph, i.e., the total number of blocks.
    */
  lazy val size = reversePostorder.length

  /**
    * The set of local variables in the control flow graph. Includes parameters (both in and out) but
    * excludes variables that are never assigned to.
    */
  lazy val localVariables : immutable.Set[chalice.Variable] =
    reversePostorder.map(_.assignedVariables.toSet).reduce(_ ++ _)
}
