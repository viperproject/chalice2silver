package ch.ethz.inf.pm.semper.chalice2sil.translation

import collection.mutable.{ArrayBuffer, Buffer, LinkedHashSet, SynchronizedSet}
import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import silAST.programs.Program
import silAST.source.SourceLocation
import java.lang.String
import silAST.programs.symbols.{FunctionFactory, PredicateFactory}
import silAST.types.DataType
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
  val programFactory = Program.getFactory(programName,programLocation)
  val methods = new FactoryHashCache[chalice.Method, MethodTranslator]{
    def construct(m : chalice.Method) = new MethodTranslator(programTranslator, m)
  }

  val fields = new DerivedFactoryCache[chalice.Field, String,  FieldTranslator]{
    def construct(field : chalice.Field) = {
      val silField = programFactory.defineField(fullFieldName(field),translateTypeExpr(field.typ))(programLocation)
      new FieldTranslator(silField,getNextId,programTranslator)
    }

    lazy val specialNameList = List(prelude.Token.joinable.name)

    override def getOrElseUpdate(p : chalice.Field) = p match {
      case chalice.SpecialField(name,_,_) if specialNameList contains name =>
        super.getOrElseUpdate(p) //TODO: do special fields need some kind of initialization?
      case _ => super.getOrElseUpdate(p)
    }

    protected def deriveKey(p : chalice.Field) = fullFieldName(p)

    override protected def deriveKeyFromValue(value : FieldTranslator) = value.field.name
  }

  val predicates = new DerivedFactoryCache[chalice.Predicate, String, PredicateTranslator] {
    protected def deriveKey(p : chalice.Predicate) = fullPredicateName(p)

    protected def construct(p : chalice.Predicate) = new PredicateTranslator(programTranslator,p,getNextId)
  }
  
  val functions = new DerivedFactoryCache[chalice.Function,  String,  FunctionTranslator] {
    protected def deriveKey(f : chalice.Function) = fullFunctionName(f)

    protected def construct(f : chalice.Function) = new FunctionTranslator(programTranslator,f)
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
    classNode.members.view foreach  {
      case f:chalice.Field => fields(f)
      case p:chalice.Predicate => predicates(p)
      case m:chalice.Method => methods(m)
      case f:chalice.Function => functions(f)
      case _ => // ignore other symbols
    }
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
      case p:chalice.Predicate =>
        predicates(p).translate()
      case f:chalice.Function =>
        functions(f).translate()
      case otherNode => report(messages.UnknownAstNode(otherNode))
    })
  }
}