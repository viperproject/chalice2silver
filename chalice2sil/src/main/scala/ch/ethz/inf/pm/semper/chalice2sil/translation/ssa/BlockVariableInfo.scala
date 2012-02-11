package ch.ethz.inf.pm.semper.chalice2sil.translation.ssa

import collection._
import silAST.programs.symbols.ProgramVariable

/**
  * @author Christian Klauser
  */
final class BlockVariableInfo(val block : ChaliceBlock, val chaliceVariable : chalice.Variable) {

  override def hashCode() = chaliceVariable.UniqueName.hashCode() ^ block.hashCode()

  override def equals(obj : Any) = obj match {
    case other:BlockVariableInfo => 
      chaliceVariable == other.chaliceVariable &&
      block == other.block
    case _ => false
  }

  /**
    * The set of blocks contributing to a ϕ-assignment for this variable.
    *
    * At this stage, instead of directly mentioning the variable version in the argument list of the
    * ϕ assignment, we just refer to the block that will supply the version. This is necessary, because
    * the variable version numbers will only be assigned afterwards, during the translation to the SIL AST.
    */
  var ϕ = immutable.Set[ChaliceBlock]()

  /**
    * Indicates whether a ϕ assignment is necessary for this variable in this block.
    */
  def needsΦAssignment = ϕ.size > 1

  /**
    * A temporary storage for the intermediate versions of a variable within a basic block. Includes `firstVersion` and
    * `lastVersion`.
    */
  var versions = immutable.IndexedSeq[Version]()

  /**
    * The version of the variable that is active at the beginning of the block. If this block has a ϕ-assignment
    * for this variable, it is a new version otherwise, it is the corresponding `lastVersion` of its predecessor.
    *
    * A block that does not assign (or ϕ assign) a variable will have `firstVersion == lastVersion`.
    */
  def firstVersion : Version = versions.head

  /**
    * The version of the variable that "leaves" the block. This is the version that is of interest to
    * successors of the block.
    *
    * A block that does not assign (or ϕ assign) a variable will have `firstVersion == lastVersion`.
    */
  def lastVersion : Version = versions.last
}
