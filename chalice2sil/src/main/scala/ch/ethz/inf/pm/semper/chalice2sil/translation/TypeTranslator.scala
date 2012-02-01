package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.programs.ProgramFactory
import silAST.methods.MethodFactory
import ch.ethz.inf.pm.semper.chalice2sil.{Message, ProgramOptions}
import collection.mutable.{SynchronizedSet, Buffer}
import silAST.types.{referenceType, permissionType, integerType, DataType}
import ch.ethz.inf.pm.semper.chalice2sil

/**
 * Author: Christian Klauser
 */

class TypeTranslator(programEnvironment : ProgramEnvironment) extends ProgramEnvironment {
  def programFactory = programEnvironment.programFactory
  def methodFactories = programEnvironment.methodFactories
  def fields  = programEnvironment.fields
  def prelude = programEnvironment.prelude
  def programOptions = programEnvironment.programOptions
  def onNewMessage = programEnvironment.onNewMessage
  def pastMessages = programEnvironment.pastMessages

  def translate(typeExpr : chalice.Type) : DataType = tryTranslateClass.orElse[chalice.Class, DataType]({
      case classRef =>
        report(chalice2sil.messages.UnknownAstNode(typeExpr)) //note the use of the original typeExpr node
        referenceType
    }).apply(typeExpr.typ)
  
  def translate(classRef : chalice.Class) : DataType = tryTranslateClass.orElse[chalice.Class,DataType]({
    case _ =>
      report(chalice2sil.messages.UnknownAstNode(classRef))  // as opposed to the generic classRef node
      referenceType
    }).apply(classRef)
  
  private val tryTranslateClass : PartialFunction[chalice.Class, DataType] = {
    case c if c == chalice.IntClass => integerType
    case c if c == chalice.BoolClass => prelude.Boolean.Type
    case x if x.IsPermission => permissionType
    case x if x.IsNormalClass => referenceType
  }
}