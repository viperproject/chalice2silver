package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.programs.ProgramFactory
import silAST.methods.MethodFactory

/**
 * Author: Christian Klauser
 * Date: 24.01.12
 */

trait ProgramEnvironment extends Environment {
  def programFactory : ProgramFactory
  def methodFactories : (chalice.Method) => MethodFactory
  def fields : (chalice.Field) => silAST.programs.symbols.Field
}