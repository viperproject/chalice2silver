package ch.ethz.inf.pm.semper.chalice2sil.translation

import collection.mutable.{ArrayBuffer, Buffer, LinkedHashSet, SynchronizedSet}
import silAST.methods.MethodFactory
import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import silAST.programs.symbols.Field
import silAST.programs.Program
import silAST.source.{noLocation, SourceLocation}
import java.lang.String
import silAST.types.{DataTypeSequence, NonReferenceDataType, referenceType}
import silAST.expressions.util.DTermSequence
import silAST.symbols.logical.Not

/**
 * Author: Christian Klauser
 */

class ProgramTranslator(val programOptions : ProgramOptions, val programName : String, val programLocation : SourceLocation) extends ProgramEnvironment {
  /**
   * The SilTranslator will call this function whenever a new message is generated.
   */
  var onNewMessage : Buffer[Message => Unit] = new ArrayBuffer
  val pastMessages = new LinkedHashSet[Message] with SynchronizedSet[Message]
  val programFactory = Program.getFactory(programLocation,programName)
  val methodFactories = new FactoryHashCache[chalice.Method,MethodFactory]{
    def construct(m : chalice.Method) = programFactory.getMethodFactory(m,fullMethodName(m))
  }
  
  val fields = new FactoryHashCache[chalice.Field, Field]{
    def construct(field : chalice.Field) = {
      val fieldName : String = fullFieldName(field)
      translate(field.typ) match {
        case t:NonReferenceDataType  => programFactory.defineDomainField(field, fieldName,t)
        case t if t == referenceType => programFactory.defineReferenceField(field,fieldName)
        case t =>
          report(messages.TypeNotUnderstood(t,field))
          programFactory.defineReferenceField(field,fieldName)
      }
    }    
  } // TODO: apply domain Field symbol for fields with domain types

  val prelude = new ChalicePrelude(this)

  def translate(decls : Seq[chalice.TopLevelDecl]) : (Program,Seq[Message]) = {
    decls.foreach(collectSymbols)
    decls.foreach(translate)
    (programFactory.getProgram,pastMessages.toSeq)
  }
  
  protected def collectSymbols(decl : chalice.TopLevelDecl) { decl match {
    case c:chalice.Class if c.IsNormalClass => collectSymbols(c)
    case node => report(messages.UnknownAstNode(node))
  }}

  protected def collectSymbols(classNode : chalice.Class){
    classNode.members.view.collect({case f:chalice.Field => f}).foreach(fields(_))
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

  protected def translate(typeExpr: chalice.Type) = {
    val translator = new TypeTranslator(this)
    translator.translate(typeExpr)
  }

}