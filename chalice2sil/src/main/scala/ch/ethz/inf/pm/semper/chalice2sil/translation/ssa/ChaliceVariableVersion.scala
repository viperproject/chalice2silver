package ch.ethz.inf.pm.semper.chalice2sil.translation.ssa

import silAST.programs.symbols.ProgramVariable

/**
  * @author Christian Klauser
  */
sealed abstract  class ChaliceVariableVersion protected (val chaliceVariable : chalice.Variable) {
  def uniqueName : String
}

final case class IntermediateChaliceVariableVersion(override val chaliceVariable : chalice.Variable, versionSuffix : String)
  extends ChaliceVariableVersion(chaliceVariable) {

  val uniqueName = chaliceVariable.UniqueName + "_" + versionSuffix
  override def toString = uniqueName
}

final case class MappedChaliceVariableVersion(override val chaliceVariable : chalice.Variable)
  extends ChaliceVariableVersion(chaliceVariable) {
  val uniqueName = chaliceVariable.UniqueName
}

object ChaliceVariableVersion {
  def intermediate(chaliceVariable : chalice.Variable, versionSuffix : String) =
    IntermediateChaliceVariableVersion(chaliceVariable, versionSuffix)
  def mapped(chaliceVariable : chalice.Variable) = MappedChaliceVariableVersion(chaliceVariable)
}
