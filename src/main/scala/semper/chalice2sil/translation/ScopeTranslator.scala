package semper.chalice2sil.translation

import semper.sil.ast.programs.symbols.ProgramVariable
import semper.chalice2sil._
import semper.sil.ast.source.{NoLocation, SourceLocation}
import collection._
import translation.util._
import semper.sil.ast.expressions.terms._
import semper.sil.ast.symbols.logical._
import semper.sil.ast.expressions._
import semper.sil.ast.types._
import semper.sil.ast.methods.implementations.{CFGFactory, BasicBlockFactory}
import semper.sil.ast.expressions.util.TermSequence
import semper.sil.ast.symbols.logical.quantification.Forall
import semper.sil.ast.expressions.UnaryExpression
import scala.Some
import semper.sil.ast.expressions.QuantifierExpression
import semper.sil.ast.symbols.logical.And
import semper.sil.ast.expressions.OldExpression
import semper.sil.ast.expressions.DomainPredicateExpression
import semper.sil.ast.symbols.logical.Or
import terms.DomainFunctionApplicationTerm
import semper.sil.ast.expressions.TrueExpression
import semper.sil.ast.symbols.logical.Implication
import terms.FieldLocation
import terms.FullPermissionTerm
import terms.NoPermissionTerm
import terms.OldTerm
import terms.PredicateLocation
import semper.sil.ast.expressions.FalseExpression
import semper.sil.ast.symbols.logical.Equivalence
import semper.sil.ast.expressions.EqualityExpression
import semper.sil.ast.expressions.BinaryExpression
import terms.ProgramVariableTerm

/**
  * @author Christian Klauser
  */
trait ScopeTranslator
  extends MemberEnvironment
  with TypeTranslator
{ thisScopeTranslator =>
  protected def cfgFactory : CFGFactory
  protected def blockStack : mutable.Stack[BasicBlockFactory]
  protected def environmentCurrentThreadVariable : ProgramVariable
  protected def environmentReadFractionVariable : ProgramVariable
  def temporaries : TemporaryVariableBroker
  def declareScopedVariable(sourceLocation : SourceLocation, uniqueName : String, dataType : DataType) : ProgramVariable

  final type CodeTranslator = ExpressionTranslator with TermTranslator with PermissionTranslator

  override def environmentReadFractionTerm(sourceLocation : SourceLocation) : Term = currentExpressionFactory.makeProgramVariableTerm(environmentReadFractionVariable,sourceLocation)
  override def environmentCurrentThreadTerm(sourceLocation : SourceLocation) : Term = currentExpressionFactory.makeProgramVariableTerm(environmentCurrentThreadVariable,sourceLocation)

  class ProgramVariableManager extends DerivedFactoryCache[chalice.Variable,  String, ProgramVariable] {
    protected def deriveKey(p : chalice.Variable) = p.UniqueName

    protected def construct(p : chalice.Variable) = declareScopedVariable(p,deriveKey(p),translateTypeExpr(p.t))

    override protected def deriveKeyFromValue(value : ProgramVariable) = value.name
  }

  def languageConstruct[T](sourceLocation : SourceLocation)(action : LanguageConstruct => T) =
    action(new LanguageConstruct(this,sourceLocation))

  def translateBody[T](body : CodeTranslator => T) {
    val methodEntry = basicBlocks("entry");
    blockStack.push(methodEntry)
    body(new MemberCodeTranslator)
    assert(blockStack.size > 0,"No block on top of the block stack after translation of body.")
    val methodExit = blockStack.pop()

    cfgFactory.setStartNode(methodEntry)
    methodExit.setHalt(NoLocation)
    cfgFactory.setEndNode(methodExit)
  }

  protected def translateStatements(codeTranslator : CodeTranslator, stmts : Traversable[chalice.Statement]){
    stmts.foreach(translateStatement(codeTranslator, _))
  }

  protected def translateStatement(codeTranslator : CodeTranslator, stmt : chalice.Statement) {
    require(!blockStack.isEmpty);
    val oldStackSize = blockStack.length
    val stackTail = blockStack.tail.toSeq

    stmt match {
      case chalice.IfStmt(guard,thn,None) =>
        val guardExpr = translateExpression(codeTranslator,guard)
        silIf(guardExpr,stmt){
          translateStatement(codeTranslator,thn)
        } end ()
      case chalice.IfStmt(guard,thn,Some(els)) =>
        val guardExpr = translateExpression(codeTranslator,guard)
        silIf(guardExpr,stmt){
          translateStatement(codeTranslator,thn)
        } els {
          translateStatement(codeTranslator,els)
        } end ()
      case chalice.BlockStmt(ss) => translateStatements(codeTranslator, ss)
      case loopNode@chalice.WhileStmt(condition,_,_,lockChange,body) =>
        translateLoop(loopNode, codeTranslator)
      case a@chalice.LocalVar(v,rhsOpt) =>
        rhsOpt match {
          case None => //nothing to do
          case Some(rhs) =>
            val variableExpr = chalice.VariableExpr(v.UniqueName)
            variableExpr.v = v
            translateAssignment(codeTranslator,variableExpr,rhs)
        }
      case a@chalice.Assert(assertion) =>
        translateAssert(assertion)
      case a@chalice.Assume(assumption) =>
        translateAssume(assumption)
      case chalice.Assign(variableExpr,rhs) => translateAssignment(codeTranslator,variableExpr,rhs)

      case chalice.FieldUpdate(location,rhs) =>
        // rhs could be an object creation. withRhsTranslation makes sure that
        //  the temporary variable used to create the object is released.
        def withRhsTranslation[T](block : Term => T) : T = rhs match {
          case newObj:chalice.NewRhs =>
            temporaries.using(referenceType){targetVar =>
              translateNew(codeTranslator,newObj,targetVar)
              block(currentExpressionFactory.makeProgramVariableTerm(targetVar,newObj))
            }
          case expr:chalice.Expression =>
            val t = translateTerm(codeTranslator, expr)
            block(t)
        }
        def assignViaVar(rcvrVar : ProgramVariable) {
          withRhsTranslation(currentBlock.appendFieldAssignment(rcvrVar, fields(location.f), _,stmt))
        }
        location.e match {
          case rcvr:chalice.VariableExpr => assignViaVar(programVariables(rcvr.v))
          case chalice.ImplicitThisExpr() => assignViaVar(thisVariable)
          case chalice.ExplicitThisExpr() => assignViaVar(thisVariable)
          case rcvr =>
            temporaries.using(referenceType){ rcvrVar =>
              val rcvrTerm = translateTerm(codeTranslator, rcvr)
              currentBlock.appendAssignment(rcvrVar,rcvrTerm,rcvr)
              withRhsTranslation(currentBlock.appendFieldAssignment(rcvrVar,fields(location.f),_,stmt))
            }
        }
      case callNode:chalice.CallAsync => translateMethodFork(callNode)
      case c:chalice.Call if c.m.isInstanceOf[chalice.Method] => translateMethodCall(c)
      case callNode:chalice.JoinAsync => translateMethodJoin(callNode)
      case foldNode:chalice.Fold => translateFold(codeTranslator, foldNode)
      case unfoldNode:chalice.Unfold => translateUnfold(codeTranslator,unfoldNode)
      case shareNode:chalice.Share =>
        translateShare(codeTranslator,shareNode)
      case unshareNode:chalice.Unshare =>
        translateUnshare(codeTranslator,unshareNode)
      case acquireNode:chalice.Acquire =>
        val objTerm = translateTerm(codeTranslator,acquireNode.obj)
        translateAcquire(codeTranslator,objTerm,acquireNode,acquireNode.obj.typ)
      case releaseNode:chalice.Release =>
        val objTerm = translateTerm(codeTranslator,releaseNode.obj)
        translateRelease(codeTranslator,objTerm,releaseNode,releaseNode.obj.typ)
      case rdAcquireNode:chalice.RdAcquire =>
        report(messages.RdLockNotSupported(rdAcquireNode))
        // to recover from this error, treat it like an ordinary acquire
        val objTerm = translateTerm(codeTranslator,rdAcquireNode.obj)
        translateAcquire(codeTranslator,objTerm,rdAcquireNode,rdAcquireNode.obj.typ)
      case rdReleaseNode:chalice.RdRelease =>
        report(messages.RdLockNotSupported(rdReleaseNode))
        // to recover from this error, treat it like an ordinary release
        val objTerm = translateTerm(codeTranslator,rdReleaseNode.obj)
        translateRelease(codeTranslator,objTerm,rdReleaseNode,rdReleaseNode.obj.typ)
      case otherStmt => report(messages.UnknownAstNode(otherStmt))
    }

    assert(blockStack.length == oldStackSize,"translate(chalice.Statement) changed the size of the blockStack when translating %s. Expected %d, actual %d"
      .format(stmt,oldStackSize,blockStack.length))
    assert(blockStack.view.drop(1).sameElements(stackTail),"translate(chalice.Statement) changed the tail elements of the blockStack when translating %s."
      .format(stmt))
  }


  protected def translateLoop(loopNode : chalice.WhileStmt, codeTranslator : ScopeTranslator.this.type#CodeTranslator) {
    val condition : chalice.Expression = loopNode.guard
    val body : chalice.BlockStmt = loopNode.body
    // A loop is represented by a single CFG node, which wraps the CFG of the loop body and carries the loop
    //  condition and invariant.
    // In many ways, a loop behaves like a method call where the pre- and postcondition is the invariant.
    // Consequently, we need to constrain a new read fraction and make sure the invariants
    // surrounding the $CurrentThread maps are upheld.

    // Step 1: Constrain a fresh read fraction variable for the loop body
    // This basically works like a method call but in order to re-use
    //  the read fraction code from method call sites, we need to take a small detour:
    // We translate the invariants (the equivalent of the precondition in the method call case)
    //  using the read fraction term of the current scope (not the loop's scope). This is because
    //  the loop read fraction term is not known at this point.
    // We use a program variable substitution to replace the (wrong) outer scope read fraction
    //  with the loop body read fraction.

    // 1.1 Use the *current* code translator to translate the invariants
    val invariantsForConstraints = loopNode.Invs.map(codeTranslator.translateExpression(_))

    // 1.2 Create the substitution for the read fraction. Have all other variables map to themselves
    val createFractionSubstitution = (loopReadFractionTerm : Term) => {
      currentExpressionFactory.makeProgramVariableSubstitution(currentBlock.programVariables.collect({
        case p if p == environmentReadFractionVariable => (p, loopReadFractionTerm)
        case p => (p, currentExpressionFactory.makeProgramVariableTerm(p, loopNode))
      }).toSet) : ProgramVariableSubstitution
    }

    val loopReadFractionVariable = determineReadPermissionFraction(
      loopNode,
      codeTranslator, // we're still in the "outer" scope (not the loop's scope)
      invariantsForConstraints,
      createFractionSubstitution,
      currentExpressionFactory.makeProgramVariableTerm(environmentCurrentThreadVariable,loopNode) // keep the current thread
    )

    // Step 2: For the $CurrentThread-related loop invariants, we need to save the state of the mu and held map
    //  at the beginning (= before) the loop
    temporaries.using(prelude.Map.HeldMap.dataType, prelude.Map.MuMap.dataType) {
      (oldHeldMapVariable, oldMuMapVariable) =>

        languageConstruct(loopNode) {
          ctor =>
            import ctor._

            oldHeldMapVariable <-- ((environmentCurrentThreadVariable:Term) ! prelude.Thread.heldMap)
            oldMuMapVariable <-- ((environmentCurrentThreadVariable:Term) ! prelude.Thread.muMap)

        }

        // Step 3: Create the loop node and the corresponding translation infrastructure ("the loop's scope")
        val loop = cfgFactory.addLoopBlock(getNextName(), translateExpression(codeTranslator, condition), loopNode)
        val loopTranslator = new
            LoopBodyTranslator(this, loop, environmentCurrentThreadVariable, loopReadFractionVariable)
        val invCodeTranslator = new loopTranslator.MemberCodeTranslator

        // Step 4: Generate $CurrentThread-related invariants
        //  Note how we translate the terms (lockchange and "old" maps) in the loop's scope.
        val currentThreadRelatedInvariants = generateThreadInvariants(loopNode,
          loopNode.lkch.map(invCodeTranslator.translateTerm(_)),
          loop.makeProgramVariableTerm(oldHeldMapVariable, loopNode),
          loop.makeProgramVariableTerm(oldMuMapVariable, loopNode)).map(x => x._1)

        // Step 5: Translate programmer supplied loop invariants again, but this time
        //  in the loop's scope. It is not enough to use our substitution from step 1,
        //  because SIL scoping also affects bound variables for use in quantifiers.
        val loopInvariants = loopNode.Invs.map(invCodeTranslator.translateExpression(_))

        // Step 6: Assign the invariant. Since SIL loops only have a single invariant expression,
        //  we concatenate our collection of invariants with &&.
        // Note: it is important that the $CurrentThread-related invariants come first,
        //  otherwise the user's conditions won't have access to the held and mu map.
        loop.setInvariant((currentThreadRelatedInvariants ++ loopInvariants)
          .reduce(currentExpressionFactory.makeBinaryExpression(And()(loopNode), _, _, loopNode)))

        // Step 7: Translate the loop's body.
        //  This happens almost in complete isolation, but it is important that the body is translated before we
        //  release our hold on the temporary variables used to hold the old states of the mu and held map.
        loopTranslator.translateBody(loopTranslator.translateStatement(_, body))

        // Step 8: As the last step, we connect the CFG nodes
        currentBlock.setGoto(loop, loopNode)
        val nextBlock = basicBlocks(getNextName("after_while"))
        loop.setGoto(nextBlock, loopNode)
        continueWith(nextBlock)
    }
  }

  protected abstract sealed class ReadCondition

  protected final case class ReadImplication(lhs : Expression, rhs : List[ReadCondition])
    extends ReadCondition
    with Product2[Expression, List[ReadCondition]] {
    def _1 = lhs
    def _2 = rhs
  }
  protected final case class ReadLocation(reference : Term, location : LocationTranslator, permissionAmount : Term) extends ReadCondition

  def allowsInexactChecking(permissionTerm : Term, methodFraction : Term, isNegative : Boolean = false) : Boolean = permissionTerm match {
    case DomainFunctionApplicationTerm(f,TermSequence(left,right)) if
      f == permissionMultiplication || f == permissionAddition =>
        allowsInexactChecking(left, methodFraction,isNegative) && allowsInexactChecking(right, methodFraction,isNegative)
    case DomainFunctionApplicationTerm(f,TermSequence(left,right)) if
      f == permissionSubtraction => allowsInexactChecking(left,methodFraction,isNegative) && allowsInexactChecking(right,methodFraction,!isNegative)
    case DomainFunctionApplicationTerm(f,TermSequence(scale,perm)) if
      f == permissionIntegerMultiplication => allowsInexactChecking(perm,methodFraction,isNegative)
    case rd if rd == methodFraction => !isNegative
    case _ => false
  }

  def filterCondByChecking(rs : List[ReadCondition], inexact : Boolean, methodFraction : Term) : List[ReadCondition] = rs.collect({
    case rf@ReadLocation(_,_,perm) if allowsInexactChecking(perm,methodFraction) == inexact => rf
    case ReadImplication(lhs,rs2) => val rs3 =
        filterCondByChecking(rs2, inexact, methodFraction).collect({
          case rf@ReadLocation(_,_,_) => rf
          case ri@ReadImplication(_,_::_) => ri //we're not interested in empty lists
        })
      ReadImplication(lhs, rs3)
  })

  def collectRdNodes(rs : List[ReadCondition]) : Seq[ReadLocation] = {
    def extract(r : ReadCondition) : Seq[ReadLocation] = r match {
      case ReadImplication(_,rs2) => rs2.flatMap(extract)
      case r2@ReadLocation(_,_,_) => List(r2)
    }
    rs.flatMap(extract)
  }

  /**
    * Produces SIL code that determines the read permission fraction (`k`) for a call (synchronous or asynchronous)
    * This method might introduce new SIL blocks into the CFG.
    * @param codeTranslator The translator to use for the receiver and the arguments.
    * @param callNode The Chalice AST node for the call. Either [[chalice.Call]] or [[chalice.CallAsync]].
    * @return the term that represents the chosen `k`.
    */
  def determineReadPermissionFraction(
                                       codeTranslator : CodeTranslator,
                                       callNode : chalice.Statement { def obj : chalice.Expression;  def args : List[chalice.Expression]; def m : chalice.Callable },
                                       newThreadTerm : Term) : ProgramVariable = {

    val calleeFactory = methods(callNode.m.asInstanceOf[chalice.Method])

    // Translate arguments and create mapping from parameter variables to these terms
    val callSubstitution = (callReadFractionTerm : Term) => {
      val argTerms =
        translateTerm (codeTranslator, callNode.obj) ::
          callNode. args.map(translateTerm(codeTranslator,_)) ++
            List(callReadFractionTerm,newThreadTerm)
      currentExpressionFactory.makeProgramVariableSubstitution(calleeFactory.parameters.zip(argTerms).map(x => x._1 -> x._2).toSet)
    }

    determineReadPermissionFraction(callNode:SourceLocation,codeTranslator,calleeFactory.method.signature.precondition,callSubstitution,newThreadTerm)
  }

  /**
    * Produces SIL code that determines the read permission fraction (`k`) for an arbitrary set of conditions.
    * This method might introduce new SIL blocks into the CFG. Some parts of the supplied condition will be
    * asserted (such as non-nullness of receivers), but no permission transfer takes place.
    * @param location The source location to use for the generated code.
    * @param codeTranslator The translator to use for the receiver and the arguments.
    * @param conditions The set of conditions act as a guide to constraining `k`.
    * @param callSubstitutionFactory Given the term for the newly constrained `k`,
    *                                creates a program variable substitution to be applied to each of the conditions.
    * @param newThreadTerm The thread the conditions belong to. If the thread doesn't change, supply the current thread.
    * @return the program variable that represents the chosen `k`.
    */
  def determineReadPermissionFraction(
    location : SourceLocation,
    codeTranslator : CodeTranslator,
    conditions : Seq[Expression],
    callSubstitutionFactory : Term => ProgramVariableSubstitution,  newThreadTerm : Term) : ProgramVariable = {

    languageConstruct(location){ ctor =>
      import ctor._
    
      //Read (fractional) permissions
      val callReadFractionVariable = declareScopedVariable(location, getNextName("k"), permissionType) // a unique variable for every method invocation
      val callReadFractionTerm : Term = callReadFractionVariable
      // `inhale 0 < k ∧ k < full ∧ (1000*k) < method_k`
      //    The factor 1000 is a hack that is also present in the Boogie-encoding.
      //    It "simulates" the fact that read permissions are really small and can be split off many, many times
      def aThousandTimes(t : Term) = currentExpressionFactory.makeDomainFunctionApplicationTerm(
        permissionIntegerMultiplication, TermSequence(1000,t),
        location,List("This \"hack\" ensures that we can give away many small read permission fractions."))
      val kPositive = permissionLT.apply(noPermission,callReadFractionTerm)
      val kOnlyRead = permissionLT.apply(aThousandTimes(callReadFractionTerm),fullPermission)
      val kSubfraction = permissionLT.apply(aThousandTimes(callReadFractionTerm),this.environmentReadFractionTerm(location))
      inhale(kPositive,kOnlyRead,kSubfraction)
  
      // Permission maps
      // `var m_0 : Map[(ref,int),Permission]`
      val originalPermMapVar = declareScopedVariable(location,getNextName("m0"),prelude.Map.PermissionMap.dataType)
      val originalPermMapTerm = originalPermMapVar : Term
      // `var m : Map[(ref,int),Permission]`
      val permMapVar = declareScopedVariable(location,getNextName("m"),prelude.Map.PermissionMap.dataType)
      val permMapTerm = permMapVar : Term
      // `inhale m = m_0`
      inhale(permMapTerm === originalPermMapTerm)
  
      val callSubstitution = callSubstitutionFactory(callReadFractionTerm)
      def transplantExpression(e : Expression) : Expression = {
        e.substitute(callSubstitution)
      }
      def transplantTerm(t : Term) : Term = {
        t.substitute(callSubstitution)
      }
  
      // Generate assumptions and conditions on fraction
      /**
        * Walks over an [[semper.sil.ast.expressions.Expression]] and extracts just read permission assertions and implications.
        * @param expr The expression to analyse.
        * @return A list of extracted read conditions.
        */
      def genReadCond(expr : Expression, isInQuantifier : Boolean = false) : List[ReadCondition] = expr match {
        case PermissionExpression(_,FullPermissionTerm()) => Nil
        case PermissionExpression(_,NoPermissionTerm()) => Nil
        case p@PermissionExpression(FieldLocation(reference:Term,field),pTerm:Term) =>
          List(ReadLocation(transplantTerm(reference),fields.lookup(field.name),transplantTerm(pTerm)))
        case p@PermissionExpression(_,LogicalVariableTerm(_))=> {
          // ignore rd*
          Nil
        }
        case p@PermissionExpression(PredicateLocation(reference:Term,pred),pTerm:Term) =>
          List(ReadLocation(transplantTerm(reference),predicates.lookup(pred.name),transplantTerm(pTerm)))
        case PermissionExpression(nonReferenceTerm,_) =>
          report(messages.ContractNotUnderstood(expr))
          Nil
        case BinaryExpression(Implication(),lhs:Expression,rhs) =>
          //use lhs as-is in implications
          List(ReadImplication(transplantExpression(lhs),genReadCond(rhs)))
        case BinaryExpression(And(),lhs,rhs) =>
          List(lhs,rhs).map(genReadCond(_)).flatten
        case BinaryExpression(Or(),lhs,rhs) =>
          report(messages.PermissionNotUnderstood(location,expr))
          Nil
        case BinaryExpression(Equivalence(),lhs,rhs) =>
          // Interpret A ↔ B ≡ (A → B) ∧ (B → A)
          genReadCond(conjunction(
            Implication()(location).t(lhs,rhs),
            Implication()(location).t(rhs,lhs)
          ))
        case  _:EqualityExpression
            | _:AtomicExpression
            | _:UnaryExpression
            | _:DomainPredicateExpression => Nil
        case QuantifierExpression(_,_,e) =>
          val c = genReadCond(e,isInQuantifier = true)
          // TODO (stefanheule): Previously, the code in comments was used, but that seems wrong to me. In particular,
          // chalice2sil introduces permissions under quantifiers, e.g. for the program peculiar.chalice.
//          if(!isInQuantifier){
//            val rds = collectRdNodes(c)
//            rds.foreach(r => report(messages.RdInQuantifier(r.reference,r.location,r.permissionAmount)))
//            Nil
//          } else {
            c
//          }
        case _ =>
          report(messages.PermissionNotUnderstood(location,expr))
          Nil
      }
  
      /**
        * Takes a list of read permission conditions as extracted by {{genReadCon}} and generates
        * the corresponding conditions on `k`.
        * @param rs the list of read permission conditions to implement.
        */
      def appendCond(rs : List[ReadCondition]){
        val combined = new DefinednessConditions(this,this.environmentReadFractionTerm(location))
        rs foreach {
          case ReadLocation(reference,field:FieldTranslator,perm) =>
            val originalPermMapTerm = currentExpressionFactory.makeProgramVariableTerm(originalPermMapVar, location)
            val permMapTerm = currentExpressionFactory.makeProgramVariableTerm(permMapVar, location)
  
            val currentActualPermission = currentExpressionFactory.makePermTerm(reference,field)(location)
            temporaries.using(prelude.Pair.Location.dataType){ locationVar =>
              val heapLocation = currentExpressionFactory.makeProgramVariableTerm(locationVar, location)

              // Assert the precondition of the reference.
              combined.visitTerm(reference,null) match {
                case TrueExpression() => // `exhale true` only confuses
                case definedness => exhale(removeSideEffects(definedness),
                  "The specification mentions a location that might not be defined. (Null reference of insufficient permission to field)",
                  reference.sourceLocation)
              }
              
              // `location := (ref,field)`
              comment("location := (ref,field) //cache a representation of memory location for access to " + field.field + ".")
              locationVar <-- field.locationLiteral(currentExpressionFactory, reference.asInstanceOf[Term])
  
              // `inhale  get(m_0,(ref,field)) = perm(ref,field)` where (ref,field) = location
              inhale((prelude.Map.PermissionMap.get.apply(originalPermMapTerm,heapLocation))===(currentActualPermission))

              val currentVirtualPermission = prelude.Map.PermissionMap.get.p(permMapTerm,heapLocation)

              // `exhale 0 < get(m,(ref,field))`
              exhale(permissionLT.apply(noPermission,currentVirtualPermission), "Permission to " + field.field + " might not be positive.")

              // `inhale perm < get(m,(ref,field))`
              inhale(permissionLT.apply(perm,currentVirtualPermission))
  
              // `m := set(m,(ref,field),get(m,(ref,field)) - perm)`
              val nextVirtualPermission = permissionSubtraction.p(currentVirtualPermission,perm)
              permMapVar <-- (prelude.Map.PermissionMap.update.p(permMapTerm,heapLocation,nextVirtualPermission))
            }
          case ReadLocation(_,p:PredicateTranslator,FullPermissionTerm()) => {
            //access with full permission can be ignored. Would fail later if illegal.
          }
          case ReadLocation(loc,p:PredicateTranslator,am) =>
            report(messages.PredicateScalingNotSupported(loc,p.predicateFactory.predicate,am))
          case _ =>
        }
  
        rs collect { case a@ReadImplication(_,_) => a } groupBy (_.lhs) foreach { i =>
          silIfGeneric(i._1,i._1.sourceLocation){
            appendCond(i._2.map(_.rhs).flatten)
          } end()
        }
      }
  
      val readConds = conditions.map(genReadCond(_))

      def emitConditionCode(allowInexact : Boolean) {
        appendCond(filterCondByChecking(readConds.flatten.toList,allowInexact,callReadFractionTerm))
      }

      comment("Collect constraints on the method call site fraction.")
      emitConditionCode(allowInexact = true)
  
      callReadFractionVariable
    }
  }

  /**
    * Translates a single Chalice statement by appending SIL statements to the current block
    * and/or creates edges to new blocks. The number of blocks on the `blockStack` is expected
    * to remain the same, but the top element might change.
    */
  def translateMethodCall(callNode : chalice.Call) {
    val codeTranslator = new MemberCodeTranslator
    val chalice.Call(_,destinations,receiver,_,args) = callNode
    val calleeFactory = methods(callNode.m.asInstanceOf[chalice.Method])

    currentBlock.appendInhale(TrueExpression()(callNode),callNode,List("Begin synchronous call to " + calleeFactory.name + "."))

    val readFractionTerm = currentExpressionFactory.makeProgramVariableTerm(determineReadPermissionFraction(codeTranslator,callNode,
      currentExpressionFactory.makeProgramVariableTerm(environmentCurrentThreadVariable,callNode)),callNode)

    // Generate call statement
    val receiverTerm = translateTerm(codeTranslator,receiver)
    val destinationVars = destinations.map(vExpr => programVariables(vExpr.v))
    val argTerms = args.map(translateTerm(codeTranslator, _)) ++
      List(readFractionTerm, currentExpressionFactory.makeProgramVariableTerm(environmentCurrentThreadVariable,callNode))
    currentBlock.appendCall(
      currentBlock.makeProgramVariableSequence(destinationVars, callNode),
      receiverTerm,
      calleeFactory,
      TermSequence(argTerms : _*),callNode,List("Perform synchronous method call to " + calleeFactory.name + "."))
  }

  def translateMethodFork(callNode : chalice.CallAsync) {
    val codeTranslator = new MemberCodeTranslator
    val chalice.CallAsync(_,chaliceTokenVariable,receiver,_,args) = callNode
    val calleeFactory = methods(callNode.m)

    languageConstruct(callNode){ ctor =>
      import ctor._

      comment("Begin asynchronous call to " + calleeFactory.methodFactory.name)

      // `token := new object`
      val tokenVar =
        if(chaliceTokenVariable == null)
          temporaries.acquire(referenceType)
        else
          programVariables(chaliceTokenVariable.v)

      val token : ProgramVariableTerm = tokenVar
      tokenVar <-- NewRef()

      // `inhale acc(token.joinable,write)`
      inhale(acc(token,prelude.Token.joinable,fullPermission))

      // `token.joinable := true`
      (tokenVar!prelude.Token.joinable) <-- booleanTrue.p()

      // Create new thread
      comment("Create object to represent new thread.")
      val threadVar = declareScopedVariable(callNode,getNextName("new_thread"),referenceType)
      val newThreadTerm = currentExpressionFactory.makeProgramVariableTerm(threadVar,callNode)
      threadVar <-- NewRef()
      inhale( acc(newThreadTerm,prelude.Thread.heldMap,fullPermission),
        acc(newThreadTerm,prelude.Thread.muMap,fullPermission)  )
      (threadVar!prelude.Thread.muMap) <-- (environmentCurrentThreadVariable!prelude.Thread.muMap.field)

      //Determine read fraction
      comment("Determine read fraction")
      val readFractionTerm  : Term = determineReadPermissionFraction(codeTranslator, callNode, newThreadTerm)

      // Store state (arguments)
      comment("Store arguments in token")
      val rcvrTerm = translateTerm(codeTranslator, receiver)
      val argTerms = rcvrTerm :: args.map(translateTerm(codeTranslator,_)) ++ List(readFractionTerm, newThreadTerm)

      argTerms.zip(calleeFactory.callToken.args) foreach { a =>
        // `inhale acc(token.field,full);`
        inhale(acc(token,a._2,fullPermission))
        // `token.field := arg`
        (tokenVar!a._2) <-- a._1
      }

      //Store state (old(*))
      comment("Store old(*) values in token")
      val callSiteSubstitution = currentExpressionFactory.makeProgramVariableSubstitution(calleeFactory.parameters.zip(argTerms).map(x => x._1 -> x._2).toSet)
      calleeFactory.callToken.oldTerms foreach { entry =>
        val oldNode = entry._1
        val tkField = entry._2
        val cp = new DefinednessConditions(this,readFractionTerm)

        // `inhale acc(tk.field,full);`
        inhale(acc(tokenVar,tkField,fullPermission))

        // `var choice : Boolean
        // `inhale eval(choice) <=> precondition(e)
        // `if(choice) { tk.field = e; }`

        oldNode match {
          case OldTermNode(OldTerm(inner:Term)) => {
            languageConstruct(inner.sourceLocation)( _ => {
              val innerLocal = inner.substitute(callSiteSubstitution)
              val precondition  = cp.visitTerm(innerLocal,null)
              silIfGeneric(precondition,innerLocal.sourceLocation){
                innerLocal match {
                  case p:Term =>
                    (tokenVar!tkField) <-- p
                    // TODO (stefanheule): which of the two cases is correct?
//                  case t:Term =>
//                    val tmp = declareScopedVariable(t.sourceLocation,getNextName("old_value"),t.dataType)
//                    inhale((tmp:Term) === t)
//                    (tokenVar!tkField) <-- tmp
                }
              } end()
            })
          }
          case OldExpressionNode(OldExpression(inner:Expression)) => { languageConstruct(inner.sourceLocation)( _ => {
            val innerLocal = inner.substitute(callSiteSubstitution)
            val precondition = cp.visitExpression(innerLocal,null)
            silIfGeneric(precondition,inner.sourceLocation){
              silIfGeneric(innerLocal,callNode){
                (tokenVar!tkField) <-- booleanTrue.p()
              } els {
                (tokenVar!tkField) <-- booleanFalse.p ()
              } end()
            } end ()
          })}
          case o => {// inner term/expression is not a program term/expression
            report(messages.ContractNotUnderstood(o.astNode))
            ()
          }
        }
      }

      // Finally: `exhale precondition(method)`, with parameters substituted
      if(calleeFactory.method.signature.precondition.size > 0)  {
        comment("Actually \"perform\" the asynchronous call by exhaling the precondition of " + calleeFactory.name + ".")
        exhale(calleeFactory.method.signature.precondition.map(_.substitute(callSiteSubstitution)),
          Some("The precondition of method " + calleeFactory.name + ", defined at " + calleeFactory.method.signature.precondition.headOption.map(_.sourceLocation).getOrElse(calleeFactory.method.sourceLocation)),
          Some(astNodeToSourceLocation(callNode)))
      }

      if(temporaries.isTemporary(tokenVar))
        temporaries.release(tokenVar)
    }
  }
  
  def translateMethodJoin(callNode : chalice.JoinAsync) {
    val codeTranslator = new MemberCodeTranslator()
    languageConstruct(callNode){ ctor =>
      import ctor._

      val (tokenVar,allocatedTemp) = translateTerm(codeTranslator, callNode.token) match {
          case ProgramVariableTerm(v) => (v,false)
        case t =>
          val v = temporaries.acquire(referenceType)
          v <-- t
          (v,true)
      }

      val tokenTerm : Term = tokenVar
      val calleeFactory = methods(callNode.m)
      val tokenStorage = calleeFactory.callToken
      val resultTargets = callNode.lhs.map(ve => programVariables(ve.v))

      comment("Begin joining of asynchronous call to method " + calleeFactory.methodFactory.name + " on token " + tokenTerm)

      // `exhale eval(token.joinable)`
      exhale(booleanEvaluate.apply(tokenTerm!prelude.Token.joinable))

      // set up substitution
      val resultTerms = tokenStorage.results.map(tokenTerm!_)
      val argumentTerms = tokenStorage.args.map(tokenTerm!_)
      val sig = calleeFactory.methodFactory.method.signature
      val (parameterVariables,resultVariables) = (sig.parameters,sig.results)
      val joinSubstitution = currentExpressionFactory.makeProgramVariableSubstitution(
        (parameterVariables.toList ++ resultVariables)
                            .zip
        (      argumentTerms       ++   resultTerms  ).toSet)

      // replace method parameters (in & out), as well as old(*) expressions with the corresponding terms at the join-site
      val trans = new ExpressionTransplantation(this) {
        def translateProgramVariable(variable : ProgramVariable) = joinSubstitution.mapVariable(variable).get

        override def transplant(expression : Expression) = expression match {
          case o@OldExpression(_) =>
            // replace `old(*)` with `eval(token.old_*)`
            val fieldRead = tokenTerm!tokenStorage.oldTerms(OldExpressionNode(o))
            booleanEvaluate.apply(fieldRead)
          case _ => super.transplant(expression)
        }

        override def transplant(term : Term) = term match {
          case o@OldTerm(_) =>
            // replace `old(*)` with `token.old_*`
            tokenTerm!tokenStorage.oldTerms(OldTermNode(o))
          case _ => super.transplant(term)
        }
      }

      // inhale postcondition (with old(*) replaced)
      comment("inhale postcondition with token fields substituted for arguments and old(*) expressions")
      comment("(also add permissions to the token fields holding the return values)")
      val methodPostcondition = sig.postcondition
        .map(trans.transplant(_))
      inhale(tokenStorage.results.map(r => acc(tokenTerm,r,fullPermission))  ++ methodPostcondition :_*)

      // assign all result fields to result variables
      comment("Assign results")
      if(tokenStorage.results.size > 0){
        resultTargets.zip(resultTerms).foreach(t => t._1 <-- t._2)
      }

      // finally set `token.joinable := false`
      comment("Set .joinable to false")
      (tokenVar!prelude.Token.joinable) <-- booleanFalse.p()

      if(allocatedTemp){
        temporaries.release(tokenVar)
      }
    }
  }

  def translateAssert(expr : chalice.Expression) {
    val translator = new MemberCodeTranslator with AssertionTranslator
    currentBlock.appendExhale(translator.translateExpression(expr),
      Some("Assertion at " + astNodeToSourceLocation(expr) + " might not hold."),
      expr,List("Assertion originally from Chalice source code."))
  }

  def translateAssume(expr : chalice.Expression) {
    val translator = new MemberCodeTranslator with AssertionTranslator
    currentBlock.appendInhale(translator.translateExpression(expr),expr,List("Assumption originally from Chalice source code. (" + astNodeToSourceLocation(expr) + ")"))
  }

  protected def translateAssignment(codeTranslator : CodeTranslator, lhs : chalice.VariableExpr,  rhs : chalice.RValue){
    val targetVariable = programVariables(lhs.v)

    rhs match {
      //chalice.RValue is (expression ∪ new-obj)
      //NewRhs  is used for both object creation and channel creation (where lower and upper bounds come into play)
      case newObj@chalice.NewRhs(typeId,init,lowerBound,upperBound) =>
        translateNew(codeTranslator,newObj,targetVariable)
      case e:chalice.Expression =>
        val rhsTerm = translateTerm(codeTranslator, e)
        currentBlock.appendAssignment(targetVariable,rhsTerm,lhs)
    }

    assert(currentBlock.programVariables contains targetVariable,
      "The SIL basic block %s is expected to have the SIL program variable %s in scope. Program variables actually in scope: {%s}"
        .format(currentBlock.name,targetVariable,currentBlock.programVariables.mkString(", ")))
  }

  /**
    * Translates an object creation node ([[chalice.NewRhs]])
    * @param codeTranslator the translator to use
    * @param newObj the new object node
    * @param targetVar the variable to assign the new object reference to
    */
  protected def translateNew(codeTranslator : CodeTranslator, newObj : chalice.NewRhs, targetVar : ProgramVariable) {
    currentBlock.appendNew(targetVar,referenceType,newObj,List("Create new object from class " + newObj.id))
    val refTerm = currentExpressionFactory.makeProgramVariableTerm(targetVar,newObj)
    val fullAccess = currentExpressionFactory.makeFullPermission(newObj)
    def addField(field : FieldTranslator) {
      currentBlock.appendInhale(
        currentExpressionFactory.makeFieldPermissionExpression(refTerm,field,fullAccess,newObj),newObj)
    }
    newObj.typ.Fields foreach { cf => addField(fields(cf)) }
    addField(prelude.Object.mu)

    newObj.initialization foreach  { init =>
      val rhsTerm = translateTerm(codeTranslator,init.e)
      currentBlock.appendFieldAssignment(targetVar,fields(init.f),rhsTerm,init)
    }

    // `inhale obj.mu == lockbottom`
    currentBlock.appendInhale(currentExpressionFactory.makeEqualityExpression(
      currentExpressionFactory.makeFieldReadTerm(refTerm,prelude.Object.mu,newObj),
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Mu().lockBottom,TermSequence(),newObj),newObj),
      newObj,List("New objects are unshared initially."))

    val target = currentBlock.makeProgramVariableTerm(targetVar,newObj)
    val currentThread = currentBlock.makeProgramVariableTerm(environmentCurrentThreadVariable,newObj)
    val currentHeldMap = currentBlock.makeFieldReadTerm(currentThread,prelude.Thread.heldMap,newObj)
    val currentMuMap = currentBlock.makeFieldReadTerm(currentThread,prelude.Thread.muMap,newObj)

    // We can safely inhale this statement, because
    //  a) obj is guaranteed to be fresh and unique
    //  b) because we never generate anything like ∀ r:ref :: $CurrentThread.heldMap[r] == true
    // `inhale $CurrentThread.helpMap[obj] == false`
    currentBlock.appendInhale(currentExpressionFactory.makeEqualityExpression(
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Map.HeldMap.get,TermSequence(currentHeldMap,target),newObj),
      currentBlock.makeDomainFunctionApplicationTerm(prelude.Boolean.falseLiteral,TermSequence(),newObj),
      newObj),newObj)

    // `inhale $CurrentThread.muMap[obj] == obj.mu`
    currentBlock.appendInhale(currentExpressionFactory.makeEqualityExpression(
      currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Map.MuMap.get,TermSequence(currentMuMap,target),newObj),
      currentBlock.makeFieldReadTerm(target,prelude.Object.mu,newObj),
      newObj),newObj)
  }

  def usingTermInVariable[T]( targetTerm : Term,
                              location : SourceLocation)(translate : ProgramVariable => T){
    targetTerm match {
      case ProgramVariableTerm(v) => translate(v)
      case term =>
        temporaries.using(referenceType){ v =>
          currentBlock.appendAssignment(v,term,location)
          translate(v)
        }
    }
  }

  def translateShare(codeTranslator : CodeTranslator, shareNode : chalice.Share){
    usingTermInVariable(translateTerm(codeTranslator,shareNode.obj),shareNode){ targetVar =>
      val targetTerm = currentExpressionFactory.makeProgramVariableTerm(targetVar,shareNode)
      languageConstruct(shareNode){ ctor =>
        import ctor._
        val currentThread = environmentCurrentThreadVariable : Term

        // `exhale target != null && target.mu == lockbottom`
        comment("Share object: " + shareNode)
        exhale(targetTerm =/= nullFunction.p(),"Object to be shared must not be null.")
        exhale(targetTerm!prelude.Object.mu === prelude.Mu().lockBottom.p(),"Object might already be shared. An unshared object has `.mu == lockbottom`")

        // check that bounds are correct (non-null and not contradicting)
        val lowerBounds = shareNode.lowerBounds.map(codeTranslator.translateTerm(_))
        val upperBounds = shareNode.upperBounds.map(codeTranslator.translateTerm(_))
        val allBounds = lowerBounds++upperBounds

        if(!allBounds.isEmpty){
          comment("Ensure that none of the bounds are null and that all their mu fields are readable.")
          for(b <- allBounds){
            exhale(b =/= nullFunction.t(),"The share bound might be null",b.sourceLocation)
            exhale(permissionLT.apply(noPermission,perm(b,prelude.Object.mu)),
              "The mu field of the share bound might not be readable.",b.sourceLocation)
          }
          comment("Ensure that all lower bounds are larger than all upper bound")
          for(lower <- lowerBounds; upper <- upperBounds){
            exhale(prelude.Mu().below.apply(lower,upper),
              "The lower bound at " + lower.sourceLocation + " might not be below the upper bound at " + upper.sourceLocation + ".",
              lower.sourceLocation)
          }
        }

        // determine mu
        val muVar = declareScopedVariable(shareNode,getNextName("fresh_mu"),prelude.Mu().dataType)
        val muTerm = muVar : Term;
        {
          comment("Determine value for fresh mu")
          inhale(prelude.Mu().below.apply(prelude.Mu().lockBottom.t(),muTerm))

          if(allBounds.isEmpty){
            // there are no bounds ↔ assume that mu is above the current waitlevel
            // `inhale ∀ o:ref :: eval($CurrentThread.heldMap[o]) ⇒ $CurrentThread.muMap[o] << mu`
            val oVar = currentExpressionFactory.makeBoundVariable(getNextName("o"),referenceType,shareNode)
            val o = currentExpressionFactory.makeBoundVariableTerm(oVar,shareNode)
            inhale(currentExpressionFactory.makeQuantifierExpression(Forall()(shareNode),oVar,
              currentExpressionFactory.makeBinaryExpression(Implication()(shareNode),
                // `eval($CurrentThread.heldMap[o])`
                prelude.Boolean.eval.apply(prelude.Map.HeldMap.get.t(currentThread!prelude.Thread.heldMap,o)),
                // `$CurrentThread.muMap[o] << mu`
                prelude.Mu().below.apply(prelude.Map.MuMap.get.t(currentThread!prelude.Thread.muMap,o),muTerm),
              shareNode)
            )(shareNode))
          } else {
            // assume that mu satisfies all bounds (we checked before that this doesn't introduce contradictions)
            for(lower <- lowerBounds){
              inhale(prelude.Mu().below.apply(lower!prelude.Object.mu,muTerm),lower.sourceLocation)
            }
            for(upper <- upperBounds){
              inhale(prelude.Mu().below.apply(muTerm,upper!prelude.Object.mu),upper.sourceLocation)
            }
          }
        }

        // Finally assign mu
        comment("Assign mu (to both the field and the map), set held to false")
        (targetVar!prelude.Object.mu) <-- muTerm
        (environmentCurrentThreadVariable!prelude.Thread.muMap) <--
          prelude.Map.MuMap.update.p(currentThread!prelude.Thread.muMap,targetTerm,targetTerm!prelude.Object.mu)
        (environmentCurrentThreadVariable!prelude.Thread.heldMap) <--
          prelude.Map.HeldMap.update.p(currentThread!prelude.Thread.heldMap,targetTerm,prelude.Boolean.falseLiteral.p())

        comment("Exhale monitor invariant")
        val monitorInvariant = monitorInvariants(shareNode.obj.typ)
        fold(targetVar,monitorInvariant,fullPermission)
        exhale(acc(targetTerm,monitorInvariant,fullPermission))
      }
    }
  }

  def translateUnshare(codeTranslator : CodeTranslator, shareNode : chalice.Unshare){
    usingTermInVariable(translateTerm(codeTranslator,shareNode.obj),shareNode){ targetVariable =>
      languageConstruct(shareNode){ ctor =>
        import ctor._
        val targetTerm = targetVariable:Term
        val currentThread = environmentCurrentThreadVariable:Term

        comment("Unshare object")
        exhale(targetTerm =/= nullFunction.p(),
          "Object to be unshared might be null",targetTerm.sourceLocation)
        exhale(permissionGE.apply(fullPermission,perm(targetTerm,prelude.Object.mu)),
          "Mu field of object to be unshared might not be writable",targetTerm.sourceLocation)
        exhale(prelude.Mu().below.apply(prelude.Mu().lockBottom.t(),targetTerm!prelude.Object.mu),
          "Object to be unshared might not be shared in the first place.",targetTerm.sourceLocation)
        exhale(prelude.Boolean.eval.apply(prelude.Map.HeldMap.get.t(currentThread!prelude.Thread.heldMap,targetTerm)),
          "Object to be unshared might not be locked.",targetTerm.sourceLocation)

        comment("Update fields/maps")
        // `obj.mu := lockbottom`
        (targetVariable!prelude.Object.mu) <-- prelude.Mu().lockBottom.p()
        // `$CurrentThread.heldMap[obj] := false`
        (environmentCurrentThreadVariable!prelude.Thread.heldMap) <--
          prelude.Map.HeldMap.update.p(currentThread!prelude.Thread.heldMap,targetTerm,prelude.Boolean.falseLiteral.p())
        // `$CurrentThread.muMap[obj] := obj.mu`
        (environmentCurrentThreadVariable!prelude.Thread.muMap) <--
          prelude.Map.MuMap.update.p(currentThread!prelude.Thread.muMap,targetTerm,targetTerm!prelude.Object.mu)
      }
    }
  }

  def translateAcquire(codeTranslator : CodeTranslator, objTerm : Term, location : SourceLocation, chaliceClass : chalice.Class){
      languageConstruct(location){ctor =>
        import ctor._

        val currentThread = environmentCurrentThreadVariable:Term

        comment("Lock an object")
        exhale(objTerm =/= nullFunction.p(),
          "Object to be locked might be null",objTerm.sourceLocation)
        exhale(permissionLT.apply(noPermission,perm(objTerm,prelude.Object.mu)),
          "Mu field of the object to be locked might not be readable.", objTerm.sourceLocation)

        // `exhale ∀ o:ref :: eval($CurrentThread.heldMap[o]) ⇒ $CurrentThread.muMap[o] << obj.mu`
        exhale(codeTranslator.withWaitlevel(location){ waitlevel =>
          prelude.Mu().below.apply(waitlevel, objTerm!prelude.Object.mu)
        },"The mu field of the object to be locked might not be above the current thread's waitlevel.")

        // Update the held map, the object is now locked
        (environmentCurrentThreadVariable!prelude.Thread.heldMap) <--
          prelude.Map.HeldMap.update.p(currentThread!prelude.Thread.heldMap,objTerm,prelude.Boolean.trueLiteral.p())

        // finally inhale and unfold the invariant predicate
        val monitorInvariant = monitorInvariants(chaliceClass)
        inhale(acc(objTerm,monitorInvariant,fullPermission))
        unfold(objTerm,monitorInvariant,fullPermission)
      }
  }

  def translateRelease(codeTranslator : CodeTranslator, objTerm : Term, location : SourceLocation, chaliceClass : chalice.Class){
    languageConstruct(location){ctor =>
      import ctor._

      val currentThread = environmentCurrentThreadVariable:Term

      comment("Release the lock on an object")
      exhale(objTerm =/= nullFunction.p(),
        "Object to be released might be null",objTerm.sourceLocation)

      exhale(prelude.Boolean.eval.apply(prelude.Map.HeldMap.get.t(currentThread!prelude.Thread.heldMap,objTerm)),
        "Object to be released might not be locked in the first place.",objTerm.sourceLocation)

      val monitorInvariant = monitorInvariants(chaliceClass)
      fold(objTerm,monitorInvariant,fullPermission)
      exhale(acc(objTerm,monitorInvariant,fullPermission))

      (environmentCurrentThreadVariable!prelude.Thread.heldMap) <--
        prelude.Map.HeldMap.update.p(currentThread!prelude.Thread.heldMap, objTerm, prelude.Boolean.falseLiteral.p())
    }
  }

  def translateLock(codeTranslator : CodeTranslator, lockNode : chalice.Lock){
    if(lockNode.rdLock){
      report(messages.RdLockNotSupported(lockNode))
      // just ignore the fact that this is a read-lock
    }

    val monitorVar = declareScopedVariable(lockNode,getNextName("lock"),referenceType)
    currentBlock.appendAssignment(monitorVar,translateTerm(codeTranslator,lockNode.obj),lockNode,List("lock statement, store monitor object in a temporary variable"))
    val monitorTerm = currentBlock.makeProgramVariableTerm(monitorVar,lockNode)
    translateAcquire(codeTranslator, monitorTerm,lockNode, lockNode.obj.typ)
    translateStatements(codeTranslator,lockNode.b.ss)
    translateRelease(codeTranslator, monitorTerm,lockNode, lockNode.obj.typ)
  }

  def translateTerm(codeTranslator : CodeTranslator, expr : chalice.Expression) : Term = {
    codeTranslator.translateTerm(expr) match {
      case pt:Term => pt
      case t =>
        assert(assertion = false,message = "Expected program term. Actual type: %s. Location: %s.".format(t.getClass, t.sourceLocation))
        null
    }
  }

  def translateExpression(codeTranslator : CodeTranslator, expr : chalice.Expression) : Expression = {
    codeTranslator.translateExpression(expr) match {
      case pe:Expression => pe
      case t =>
        assert(assertion = false,message = "Expected program expression. Actual type: %s. Location: %s.".format(t.getClass, t.sourceLocation))
        null
    }
  }

  def translateFold(codeTranslator : CodeTranslator, foldNode : chalice.Fold) {
    val predicateAccess = foldNode.pred
    val location = codeTranslator.translateTerm(predicateAccess.ma.e)
    currentBlock.appendFold(location,predicates(predicateAccess.ma.predicate),codeTranslator.translatePermission(predicateAccess.perm),foldNode)
  }

  def translateUnfold(codeTranslator : CodeTranslator, unfoldNode : chalice.Unfold) {
    val predicateAccess = unfoldNode.pred
    val location = codeTranslator.translateTerm(predicateAccess.ma.e)
    currentBlock.appendUnfold(
      currentExpressionFactory.makePredicatePermissionExpression(
        location,
        predicates(predicateAccess.ma.predicate),
        codeTranslator.translatePermission(predicateAccess.perm),
        unfoldNode),
      unfoldNode)
  }

  class MemberCodeTranslator extends DefaultCodeTranslator(thisScopeTranslator) {
    override protected def readFraction(location : SourceLocation) = environmentReadFractionTerm(location)
  }

  protected def generateThreadInvariants(
                                          location : SourceLocation,
                                          lockChanged : scala.List[Term],
                                          oldHeldMap : Term,
                                          oldMuMap : Term) : Seq[(Expression,SourceLocation)] = {
    pureLanguageConstruct(location) {
      ctor =>
        import ctor._

        val mapStateConditions = mutable.ArrayBuffer[(Expression,SourceLocation)]()

        val heldMapTerm : Term = environmentCurrentThreadTerm(location)!prelude.Thread.heldMap
        val muMapTerm : Term = environmentCurrentThreadTerm(location)!prelude.Thread.muMap

        val currentThreadTerm = environmentCurrentThreadTerm(location)

        // Handle $CurrentThread
        // requires $CurrentThread != null && acc($CurrentThread.heldMap) && acc($CurrentThread.muMap)
        val heldMapAccess = acc(currentThreadTerm, prelude.Thread.heldMap, fullPermission)
        val muMapAccess = acc(currentThreadTerm, prelude.Thread.muMap, fullPermission)

        // ensures acc($CurrentThread.heldMap) && acc($CurrentThread.muMap)
        mapStateConditions += ((conjunction(heldMapAccess, muMapAccess), location))

        // Add constraints about currentThread.heldMap and currentThread.muMap
        def forallReferencesInPostcondition(f : Function[Term, Expression]) = {
          val qObjVar = currentExpressionFactory.makeBoundVariable(getNextName("o"), referenceType, location)
          val q : Expression = currentExpressionFactory.makeQuantifierExpression(Forall()(location), qObjVar,
            f(currentExpressionFactory.makeBoundVariableTerm(qObjVar, location))
          )(location, Nil)
          mapStateConditions.+=((q, location))
        }

        def isLockChanged(term : Term) : Expression = {
          lockChanged.map(lc => currentExpressionFactory.makeEqualityExpression(term, lc, lc.sourceLocation) : Expression).
            reduceOption((l, r) => currentExpressionFactory.makeBinaryExpression(And()(l.sourceLocation), l, r, l.sourceLocation)).
            getOrElse(FalseExpression()(term.sourceLocation, Nil))
        }
        def exceptIsLockChanged(obj : Term, expr : Expression) = {
          isLockChanged(obj) match {
            case FalseExpression() => expr
            case test =>
              currentExpressionFactory.makeBinaryExpression(Implication()(test.sourceLocation),
                currentExpressionFactory.makeUnaryExpression(Not()(test.sourceLocation), test, test.sourceLocation),
                expr,
                test.sourceLocation)
          }
        }

        def heldLookup(ref : Term, customHeldMapTerm : Option[Term] = None) : Term = {
          currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Map.HeldMap.get,
            TermSequence(customHeldMapTerm.getOrElse(heldMapTerm), ref), ref.sourceLocation)
        }
        def muLookup(ref : Term, customMuMapTerm : Option[Term] = None) : Term = {
          currentExpressionFactory.makeDomainFunctionApplicationTerm(prelude.Map.MuMap.get,
            TermSequence(customMuMapTerm.getOrElse(muMapTerm), ref), ref.sourceLocation)
        }

        //  ∀ o : ref :: ¬isLockChanged(o) ⇒ old(currentThread.heldMap)[o] == currentThread.heldMap[o]
        forallReferencesInPostcondition(o => {
          exceptIsLockChanged(o,
            currentExpressionFactory.makeEqualityExpression(
              heldLookup(o, Some(oldHeldMap)),
              heldLookup(o), location
            ))
        })

        // ∀ o : ref :: ¬isLockChanged(o) ⇒ (currentThread.heldMap[o] ⇒ old(currentThread.muMap)[o] == currentThread.muMap[o]))
        forallReferencesInPostcondition(o => {
          exceptIsLockChanged(o,
            currentExpressionFactory.makeBinaryExpression(Implication()(location),
              currentExpressionFactory.makeDomainPredicateExpression(prelude.Boolean.eval, TermSequence(heldLookup(o)), location),
              currentExpressionFactory.makeEqualityExpression(
                muLookup(o, Some(oldMuMap)),
                muLookup(o),
                location),
              location, Nil
            )
          )
        }
        )

        mapStateConditions
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////      HELPER FUNCTIONS (TRANSLATION DSL)                              /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * A condition with a general (possibly non-program) expression as it's guard.
    * @param cond The guard of the condition
    * @param condLocation The source location for this if-statement
    * @param thnBlock Function that generates the code for the "then" block
    * @tparam T The type of the value computed by the "then"-block generating code.
    * @return An object with two methods: "end" and "els". Code will only be generated when either of them is called.
    */
  protected def silIfGeneric[T](cond : Expression, condLocation : SourceLocation)(thnBlock : => T) = new {
    protected val conditionLocation = if(condLocation == NoLocation) Some(cond.sourceLocation) else Some(condLocation)

    def els[U](elsBlock : => U) = new {
      def end() : (T,Option[U]) = {
        cond match {
          case p:Expression => translateSilCondition[T,U](p,()=>thnBlock,Some(() => elsBlock),conditionLocation,None)
          case _ =>  translateGenericSilCondition(Some(() => elsBlock))
        }
      }
    }
    def end() : T = {
      val (t,_) = cond match {
        case p:Expression => translateSilCondition[T,Unit](p,() => thnBlock,None,conditionLocation,None)
        case _ => translateGenericSilCondition(None)
      }
      t
    }
    def translateGenericSilCondition[U](elsBlock : Option[()=>U]) : (T,Option[U]) = {
      // inhale eval(choice) <=> cond
      // if(eval(choice)) { ... } else { ... }
      val choiceVar = declareScopedVariable(cond.sourceLocation,getNextName("if"),prelude.Boolean.dataType)
      val choiceTerm = currentExpressionFactory.makeProgramVariableTerm(choiceVar,cond.sourceLocation)
      val choiceExpr : Expression = currentExpressionFactory.makeDomainPredicateExpression(prelude.Boolean.eval,TermSequence(choiceTerm),cond.sourceLocation)
      currentBlock.appendInhale(
        currentExpressionFactory.makeBinaryExpression(Equivalence()(cond.sourceLocation),
          choiceExpr,
          removeSideEffects(cond),cond.sourceLocation),cond.sourceLocation,List("Bind non-program expression to program variable for use in condition."))
      translateSilCondition[T,U](choiceExpr,() => thnBlock,elsBlock,conditionLocation,None)
    }
  }

  protected def silIf[T](cond : Expression, condLocation : SourceLocation = NoLocation)(thnBlock : => T) = new {
    protected val conditionLocation = if(condLocation == NoLocation) Some(cond.sourceLocation) else Some(condLocation)

    def els[U](elsBlock : => U) = new {
      def end() = {
        translateSilCondition[T, U](cond,() => thnBlock,Some(() => elsBlock),conditionLocation)
      }
    }
    def end() : T = {
      val (t,_) = translateSilCondition[T,Unit](cond,() => thnBlock,None,conditionLocation)
      t
    }
  }

  def translateSilCondition[T,U](
                                  condExpr : Expression,
                                  thn : () => T,
                                  elsOpt : Option[() => U] = None,
                                  thnLoc : Option[SourceLocation] = None,
                                  elsLoc : Option[SourceLocation] = None) : (T, Option[U]) = {

    val thenLocation = thnLoc.getOrElse(NoLocation)
    val elseLocation = elsLoc.getOrElse(NoLocation)

    //Create block for then-branch. (else is only created when necessary)
    val thenBlock = basicBlocks(getNextName("if_then"))

    //  compile body of then-branch
    val (thenResult,endThenBlock) = into(thenBlock,thn())

    // IMPORTANT: do not create the nextBlock until both branches are compiled
    //  otherwise, temporary variables declared in else might not known!
    lazy val nextBlock = basicBlocks(getNextName("if_continue"))

    //Handle else-block if there is one. "elseSuccessor" is the block the control should be transferred to
    //  when the condition is false. This is either the actual successor block or the else-branch

    val (elseResult,elseSuccessor) = elsOpt match {
      case Some(els) =>
        //Compile else-branch, same as then-branch
        val elseBlock = basicBlocks(getNextName("if_else"))
        val (result,elseBlockEnd) = into(elseBlock,els())

        elseBlockEnd.setGoto(nextBlock,elseLocation)
        (Some(result),elseBlock)
      case None => (None,nextBlock)
    }

    //  finally, connect then-block to successor with no condition (True)
    endThenBlock.setGoto(nextBlock,thenLocation)

    //Create control transfer from current block to then or else
    currentBlock.setBranch(condExpr,thenBlock,elseSuccessor,thenLocation)

    //Update currentBlock
    this.continueWith(nextBlock)

    (thenResult,elseResult)
  }

  def basicBlocks = new FactoryHashCache[String,  BasicBlockFactory]{
    protected def construct(key : String) = cfgFactory.addBasicBlock(key,NoLocation)
  }

  def currentBlock = {
    require(!blockStack.isEmpty,"Attempted to access \"current block\" outside of method body.")
    blockStack.top
  }

  /**
    * Runs a translation with the specified block as the currentBlock. Will then ensure that
    * the block that ends up as the currentBlock at the end of the translation does not have any
    * outgoing edges already, i.e., is open for additional statements.
    *
    * The typical pattern for using `into` is as follows:
    * {{{
    * val targetBlockBegin = basicBlocks(getNextName("target_block"))
    * val (result,targetEndBlock) = into(targetBlockBegin, myTranslation(x))
    * }}}
    * At this point `targetBlockBegin` and `targetBlockEnd` might be the same block, or there might be
    * an arbitrary number of block in between.
    *
    * The end block is guaranteed not to have any outgoing edges, so you can safely append statements
    * that are meant to follow `myTranslation(x)`.
    */
  protected def into[T](blockHead : BasicBlockFactory, translation : =>  T) : (T,BasicBlockFactory) = {
    blockStack push blockHead
    val result = translation
    val blockEnd = blockStack.pop()
    (result,blockEnd)
  }

  protected def continueWith(block : BasicBlockFactory) = {
    require(!blockStack.isEmpty)

    blockStack.pop()
    blockStack.push(block)
  }
}
