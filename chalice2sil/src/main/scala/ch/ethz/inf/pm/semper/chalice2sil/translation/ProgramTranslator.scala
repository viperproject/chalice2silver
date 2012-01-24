package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.source.{SourceLocation}
import collection.mutable.{ArrayBuffer, Buffer, LinkedHashSet, SynchronizedSet}
import silAST.programs.{Program => SilProgram}
import silAST.methods.MethodFactory
import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import silAST.programs.symbols.Field

/**
 * Author: Christian Klauser
 */

class ProgramTranslator(val programOptions : ProgramOptions, val programName : String, val programLocation : SourceLocation) extends ProgramEnvironment {
  /**
   * The SilTranslator will call this function whenever a new message is generated.
   */
  var onNewMessage : Buffer[Message => Unit] = new ArrayBuffer
  val pastMessages = new LinkedHashSet[Message] with SynchronizedSet[Message]
  def programFactory = SilProgram.getFactory(programLocation,programName)
  def methodFactories = FactoryCache.apply[chalice.Method,MethodFactory](m => programFactory.getMethodFactory(m,fullMethodName(m)))
  def fields = FactoryCache.apply[chalice.Field, Field](f => programFactory.defineReferenceField(f,fullFieldName(f))) // TODO: apply domain Field symbol for fields with domain types

  def translate(decls : Seq[chalice.TopLevelDecl]) : (SilProgram,Seq[Message]) = {
    val pf = SilProgram.getFactory(programLocation,programName)

    decls.foreach(translate)

    (pf.getProgram,pastMessages.toSeq)
  }

  protected def translate(decl : chalice.TopLevelDecl) {decl match {
    case c:chalice.Class if c.IsNormalClass => translate(c)
    case c:chalice.Channel => report(messages.ChannelsNotImplemented(c))
    case node => report(messages.UnknownAstNode(node))
  }}

  protected def translate(classNode: chalice.Class){
    classNode.members.foreach({
      case m:chalice.Method  =>
        val translator = new MethodTranslator(this,m)
        translator.translate
      case f:chalice.Field => // nothing to do for fields at this time
      case otherNode => report(messages.UnknownAstNode(otherNode))
    })
  }


}