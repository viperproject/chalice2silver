package ch.ethz.inf.pm.semper.chalice2sil.translation.ssa

import silAST.programs.symbols.ProgramVariable

/**
  * @author Christian Klauser
  */
final class ChaliceVariableVersion(val chaliceVariable : chalice.Variable, val versionSuffix : String) {
  override def hashCode() = chaliceVariable.hashCode() ^ versionSuffix.hashCode()

  override def equals(obj : Any) = obj match {
    case c:ChaliceVariableVersion => versionSuffix == c.versionSuffix && chaliceVariable == c.chaliceVariable
    case _ => false
  }

  val uniqueName = chaliceVariable.UniqueName + "_" + versionSuffix

  override def toString = uniqueName + ": " + chaliceVariable.t
}
