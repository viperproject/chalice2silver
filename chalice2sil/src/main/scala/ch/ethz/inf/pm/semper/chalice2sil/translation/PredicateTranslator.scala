package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import silAST.programs.symbols.PredicateFactory

/**
  * @author Christian Klauser
  */
class PredicateTranslator(environment : ProgramEnvironment, val predicate : chalice.Predicate)
  extends DerivedProgramEnvironment(environment)
  with MemberEnvironment
{
  def programVariables = null

  def thisVariable = null

  def readFractionVariable = null

  def currentExpressionFactory = null

  def nameSequence = util.NameSequence()

  val predicateFactory : PredicateFactory = null
}
