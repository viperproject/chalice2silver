package ch.ethz.inf.pm.semper.chalice2sil.translation.ssa

/**
  * Represents an edge in a chalice control flow graph, or a transfer of control between [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.ChaliceBlock]]s.
  * @author Christian Klauser
  */
final case class ChaliceEdge private[ssa]
( /**
    * The [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.ChaliceBlock]] that this edge transfers control from.
    */
  origin : ChaliceBlock,

  /**
    * The [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.ChaliceBlock]] that this edge transfers control to.
    */
  destination : ChaliceBlock,

  /**
    * If the transfer of control only occurs under a certain condition, 
    * this field holds the corresponding chalice expressions. The elements of this list will appear in a
    * logical conjunction in the SIL program.
    */
  condition : List[chalice.Expression] = Nil,

  /**
    * Indicates whether the [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.ChaliceEdge.condition]] is to be
    * inverted. We can't just use Chalice's logical [[chalice.Not]] because SIL distinguishes between not on the
    * predicate level [[silAST.symbols.logical.Not]] and the not operator for values of the Boolean value domain.
    */
  isInverted : Boolean = false,

  /**
    * Indicates that an edge points back to an "earlier" block in the control flow graph, forming a loop.
    * This annotation is carried over to the SIL control flow graph.
    */
  isBackEdge : Boolean = false);
