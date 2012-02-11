package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil.ChaliceSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.{MatchResult, BeMatcher, ShouldMatchers}
import silAST.methods.implementations.FieldAssignmentStatement
import silAST.types.{referenceType, integerType}
import silAST.expressions.PermissionExpression
import silAST.expressions.terms.{ProgramVariableTerm, fullPermissionTerm, LiteralTerm}
import silAST.programs.symbols.ProgramVariable

@RunWith(classOf[JUnitRunner])
class Ssa extends ChaliceSuite with ShouldMatchers {

}
