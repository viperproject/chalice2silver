package ch.ethz.inf.pm.semper.chalice2sil.translation

/**
  * @author Christian Klauser
  */
package object ssa {
  implicit def wrapChaliceStatement(stmt : chalice.Statement) = new {
    import collection.immutable._
    
    def assignedVariables = stmt.Targets
  }

  type Version = ChaliceVariableVersion
}
