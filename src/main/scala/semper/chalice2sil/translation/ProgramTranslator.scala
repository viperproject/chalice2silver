package semper.chalice2sil.translation

import semper.sil.ast._
import semper.chalice2sil.util._
import scala.collection.mutable._
import java.lang.String
import semper.chalice2sil.messages._
import collection.mutable

/**
 * Author: Christian Klauser
 * Code modified by Yannis Kassios
 */

class ProgramTranslator(val programOptions : semper.chalice2sil.ProgramOptions, val programName : String)
  extends RealPosition
{
  override val line = 0
  override val column = 0

  // output of the translator
  val messages = new LinkedList[MessageId]
  val silEnvironment = new SILProgramEnvironment

  // sequence and set domains
  val typeVar = new TypeVar("X")
  val seqDomain =
    new Domain("seq", Nil, Nil, List(typeVar))
    // YANNIS todo: add sequence functions and axioms
  val setDomain =
    new Domain("set", Nil, Nil, List(typeVar))
    // YANNIS todo: add set functions and axioms


  // symbols of the Chalice program
  val chaliceEnvironment = new ChaliceProgramEnvironment

  /*val monitorInvariants = new DerivedFactoryCache[chalice.Class, String, PredicateTranslator] {
    protected def deriveKey(c : chalice.Class) = fullMonitorInvariantName(c)
    protected def construct(c : chalice.Class) = new MonitorInvariantPredicateTranslator(programTranslator,c,getNextId)
  }*/
  // YANNIS: todo: how are monitor invariants translated?

  val prelude = new ChalicePrelude(this)
  // YANNIS todo: fix ChalicePrelude

  def translate(decls : Seq[chalice.TopLevelDecl]) : (Program,LinkedList[MessageId]) = {
    decls.foreach(collectSymbols)
    decls.foreach(translate)
    (new Program(programName, List(seqDomain, setDomain), silEnvironment.silFields, silEnvironment.silFunctions,
      silEnvironment.silPredicates, silEnvironment.silMethods), messages)
  }
  
  protected def collectSymbols(decl : chalice.TopLevelDecl) { decl match {
    case c:chalice.Class if c.IsNormalClass => collectSymbols(c)
    case node => messages += UnknownAstNode(node)
  }}

  protected def collectSymbols(classNode : chalice.Class){
    classNode.members.view foreach  {
      case f:chalice.Field =>
        chaliceEnvironment.chaliceFields += (f.id, f)
        silEnvironment.silFields += (f.FullName, translateType(f.typ))
      case p:chalice.Predicate =>
        chaliceEnvironment.chalicePredicates += (p.id, p)
        silEnvironment.silPredicates += (p.FullName, null)(new SourcePosition(p.pos.line, p.pos.column))
      case m:chalice.Method =>
        chaliceEnvironment.chaliceMethods += (m.id, m)
        // YANNIS: todo: create SIL Method object here: silEnvironment.silMethods += (m.FullName, ...)
        // Problem: pre/postconditions must be translated before the object is constructed.  must talk to Stefan
      case f:chalice.Function =>
        chaliceEnvironment.chaliceFunctions += (f.id, f)
        // YANNIS: todo: same problem here: silEnvironment.silFunctions += (f.FullName, ...)
      case i:chalice.MonitorInvariant =>  // makes sure that invariant is created lazily
      case _ => // ignore other symbols
    }
  }

  protected def translate(decl : chalice.TopLevelDecl) {decl match {
    case c:chalice.Class if c.IsNormalClass => translate(c)
    case c:chalice.Channel => messages += ChannelsNotImplemented(c)
    case node => messages += UnknownAstNode(node)
  }}


  protected def translate(classNode: chalice.Class) = {
    // In Chalice2SIL, monitor invariants are not considered "members of a class"
    //  they are a "property of a class"

    // Translate one member at a time
    classNode.members.foreach({
      case m:chalice.Method  =>
        new MethodTranslator(this, m).translate() // YANNIS todo: fix method translators
      case f:chalice.Field =>
        new FieldTranslator(this, f).translate() // YANNIS todo: fix field translators
      case p:chalice.Predicate =>
        new PredicateTranslator(this, p).translate() // YANNIS todo: fix predicate translators
      case f:chalice.Function =>
        new FunctionTranslator(this, f).translate() // YANNIS todo: fix function translators
      case i:chalice.MonitorInvariant => () // YANNIS: todo: how are monitor invariants treated?
      case otherNode => messages += UnknownAstNode(otherNode)
    })
  }

  protected def translateType(typ: chalice.Type) {
    typ.id match {
      case "seq" =>
        if(typ.params.length != 1) { messages += WrongNumberOfTypeParameters; return Int }
        val tvm = new HashMap[TypeVar, Type]
        val silT = translateType(typ.params.head)
        tvm += (typeVar, silT)
        new DomainType(seqDomain, tvm)
      case "set" =>
        if(typ.params.length != 1) { messages += WrongNumberOfTypeParameters; return Int }
        val tvm = new HashMap[TypeVar, Type]
        val silT = translateType(typ.params.head)
        tvm += (typeVar, silT)
        new DomainType(setDomain, tvm)
      case "int" => Int
      case "bool" => Bool
      case _ => Ref
    }
  }
}