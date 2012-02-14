package ch.ethz.inf.pm.semper.chalice2sil.translation.ssa

import collection._
import silAST.programs.symbols.ProgramVariable

/**
  * @author Christian Klauser
  */
final class BlockVariableInfo(val block : ChaliceBlock, val variable : chalice.Variable) {

  override def hashCode() = variable.UniqueName.hashCode() ^ block.hashCode()
  override def toString = variable.UniqueName + (if(needsΦAssignment)  "ϕ(" + ϕ.map(_.uniqueName).mkString(", ") + ")" else "") +
    " versions(" + versions.map(_.uniqueName).mkString(", ") + ")"

  override def equals(obj : Any) = obj match {
    case other:BlockVariableInfo => 
      variable == other.variable &&
      block == other.block
    case _ => false
  }

  /**
    * The set of versions contributing to the ϕ assignment to the `variable` at the beginning of the `block`.
    * Is assigned by [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.SsaSurvey.determineDefinitionReach]]
    */
  var ϕ = immutable.Set[Version]()

  /**
    * Indicates whether a ϕ assignment is necessary for this variable in this block. 
    * Is assigned by [[ch.ethz.inf.pm.semper.chalice2sil.translation.ssa.SsaSurvey.determineΦLocations]]
    */
  var needsΦAssignment = false

  def eliminatedΦAssignment = !needsΦAssignment && hasΦAssignment

  def hasΦAssignment = !ϕ.isEmpty

  /**
    * Indicates whether the variable inherits its value directly from the blocks preceding it. False does not
    * mean that the variable is independent of any previous versions.
    */
  def inheritsValue = !needsΦAssignment && !(block.initializedVariables contains variable)

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
