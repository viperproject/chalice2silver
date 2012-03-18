package ch.ethz.inf.pm.semper.chalice2sil.translation

import collection.mutable.{ArrayBuffer, Buffer, LinkedHashSet, SynchronizedSet}
import silAST.methods.MethodFactory
import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import silAST.programs.Program
import silAST.source.{noLocation, SourceLocation}
import java.lang.String
import silAST.expressions.util.DTermSequence
import silAST.symbols.logical.Not
import silAST.programs.symbols.{FunctionFactory, PredicateFactory, Predicate, Field}
import silAST.types.{DataType, DataTypeSequence, NonReferenceDataType, referenceType}
import java.util.concurrent.atomic.AtomicInteger
import util.{DerivedFactoryCache, FactoryHashCache}

/**
 * Author: Christian Klauser
 */

class ProgramTranslator(val programOptions : ProgramOptions, val programName : String, val programLocation : SourceLocation)
    extends ProgramEnvironment
    with TypeTranslator { programTranslator =>
  /**
   * The SilTranslator will call this function whenever a new message is generated.
   */
  var onNewMessage : Buffer[Message => Unit] = new ArrayBuffer
  val pastMessages = new LinkedHashSet[Message] with SynchronizedSet[Message]
  val programFactory = Program.getFactory(programLocation,programName)
  val methods = new FactoryHashCache[chalice.Method, MethodTranslator]{
    def construct(m : chalice.Method) = new MethodTranslator(programTranslator, m)
  }

  val fields = new DerivedFactoryCache[chalice.Field, String,  FieldTranslator]{
    def construct(field : chalice.Field) = {
      val silField = programFactory.defineField(noLocation,fullFieldName(field),translateTypeExpr(field.typ))
      new FieldTranslator(silField,getNextId,programTranslator)
    }

    protected def deriveKey(p : chalice.Field) = fullFieldName(p)

    override protected def deriveKeyFromValue(value : FieldTranslator) = value.field.name
  }

  val predicates = new DerivedFactoryCache[chalice.Predicate, String, PredicateFactory] {
    protected def deriveKey(p : chalice.Predicate) = fullPredicateName(p)

    protected def construct(p : chalice.Predicate) = programFactory.getPredicateFactory(p,fullPredicateName(p))
  }
  
  val functions = new DerivedFactoryCache[chalice.Function,  String,  FunctionFactory] {
    protected def deriveKey(f : chalice.Function) = fullFunctionName(f)

    protected def construct(f : chalice.Function) = {
      val params = f.ins.map[(SourceLocation,String, DataType),Seq[(SourceLocation,String, DataType)]](
        v => (v,v.id,translateTypeExpr(v.t)))
      programFactory.getFunctionFactory(f,fullFunctionName(f),params,translateTypeExpr(f.out))
    }
  }

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
        methods(m).translate()
      case f:chalice.Field => // nothing to do for fields at this time
      case otherNode => report(messages.UnknownAstNode(otherNode))
    })
  }
}