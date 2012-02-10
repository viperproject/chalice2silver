package ch.ethz.inf.pm.semper.chalice2sil.translation.cfg

import scala.collection._;

/**
  * A control flow graph for a chalice method. This data structure is used
  * to cache the results of potentially expensive algorithms. It assumes that
  * the **structure** (edges) of the graph remains static while the object is in use.
  * 
  * @author Christian Klauser
  */
class ControlFlowSketch(val entryBlock : ChaliceBlock, val exitBlock : ChaliceBlock) {
  require(entryBlock.predecessors.isEmpty)
  require(exitBlock.successors.isEmpty)

  lazy val postorder : Array[ChaliceBlock] = {
    val builder = mutable.ArrayBuilder.make[ChaliceBlock]()

    def visitPostorder(block : ChaliceBlock){
      block.successors.view
        .filter(!_.isBackEdge)
        .map(_.destination)
        .foreach(visitPostorder)

      builder += block
    }

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
}
