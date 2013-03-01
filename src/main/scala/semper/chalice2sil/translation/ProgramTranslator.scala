package semper.chalice2sil.translation

import collection._
import semper.chalice2sil
import chalice2sil._
import semper.sil.ast.programs.Program
import semper.sil.ast.source.SourceLocation
import java.lang.String
import util.{DerivedFactoryCache, FactoryHashCache}

/**
 * Author: Christian Klauser
 */

class ProgramTranslator(val programOptions : ProgramOptions, val programName : String, val programLocation : SourceLocation)
    extends ProgramEnvironment
    with TypeTranslator { programTranslator =>
  /**
   * The individual translators will call this function whenever a new message is generated.
   */
  var onNewMessage : mutable.Buffer[Message => Unit] = new mutable.ArrayBuffer
  val pastMessages = new mutable.LinkedHashSet[Message] with mutable.SynchronizedSet[Message]
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

    protected def construct(p : chalice.Predicate) = new ChalicePredicateTranslator(programTranslator,p,getNextId)
  }
  
  val functions = new DerivedFactoryCache[chalice.Function,  String,  FunctionTranslator] {
    protected def deriveKey(f : chalice.Function) = fullFunctionName(f)

    protected def construct(f : chalice.Function) = new FunctionTranslator(programTranslator,f)
  }

  val monitorInvariants = new DerivedFactoryCache[chalice.Class, String, PredicateTranslator] {
    protected def deriveKey(c : chalice.Class) = fullMonitorInvariantName(c)
    protected def construct(c : chalice.Class) = new MonitorInvariantPredicateTranslator(programTranslator,c,getNextId)
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
      case f:chalice.Field =>
        fields(f)
        if (f.isTracked) fields(
          new chalice.Field("~(" + classNode.id + "." + f.id + ")", new chalice.Type("set", List(new chalice.Type(classNode))), false, false)
        )
      case p:chalice.Predicate => predicates(p)
      case m:chalice.Method => methods(m)
      case f:chalice.Function => functions(f)
      case i:chalice.MonitorInvariant =>  // makes sure that invariant is created lazily
      case _ => // ignore other symbols
    }
  }

  protected def translate(decl : chalice.TopLevelDecl) {decl match {
    case c:chalice.Class if c.IsNormalClass => translate(c)
    case c:chalice.Channel => report(messages.ChannelsNotImplemented(c))
    case node => report(messages.UnknownAstNode(node))
  }}

  protected def translate(classNode: chalice.Class){
    // In Chalice2SIL, monitor invariants are not considered "members of a class"
    //  they are a "property of a class"

    // Translate one member at a time
    classNode.members.foreach({
      case m:chalice.Method  =>
        methods(m).translate()
      case f:chalice.Field => // nothing to do for fields at this time
      case p:chalice.Predicate =>
        predicates(p).asInstanceOf[ChalicePredicateTranslator].translate()
      case f:chalice.Function =>
        functions(f).translate()
      case i:chalice.MonitorInvariant => () //handled separately
      case otherNode => report(messages.UnknownAstNode(otherNode))
    })
  }
}