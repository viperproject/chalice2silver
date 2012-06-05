package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.programs.symbols.ProgramVariable
import ch.ethz.inf.pm.semper.chalice2sil._
import silAST.source.{noLocation, SourceLocation}
import collection.mutable.Stack
import translation.util._
import silAST.expressions.terms._
import silAST.symbols.logical.{Equivalence, Or, And, Implication}
import silAST.expressions._
import silAST.types._
import silAST.methods.implementations.{CFGFactory, BasicBlockFactory}
import silAST.expressions.util.{TermSequence, PTermSequence}

/**
  * @author Christian Klauser
  */
trait ScopeTranslator
  extends MemberEnvironment
  with TypeTranslator
{ thisScopeTranslator =>
  def cfgFactory : CFGFactory
  def blockStack : Stack[BasicBlockFactory]
  def temporaries : TemporaryVariableBroker
  def declareScopedVariable(sourceLocation : SourceLocation, uniqueName : String, dataType : DataType) : ProgramVariable

  final type CodeTranslator = ExpressionTranslator with TermTranslator with PermissionTranslator

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
    methodExit.setHalt(noLocation)
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
        val guardExpr = translatePExpression(codeTranslator,guard)
        silIf(guardExpr,stmt){
          translateStatement(codeTranslator,thn)
        } end ()
      case chalice.IfStmt(guard,thn,Some(els)) =>
        val guardExpr = translatePExpression(codeTranslator,guard)
        silIf(guardExpr,stmt){
          translateStatement(codeTranslator,thn)
        } els {
          translateStatement(codeTranslator,els)
        } end ()
      case chalice.BlockStmt(ss) => translateStatements(codeTranslator, ss)
      case loopNode@chalice.WhileStmt(condition,_,_,lockChange,body) =>
        val conditionExpr = translatePExpression(codeTranslator,condition)
        //compile loop body
        val loop = cfgFactory.addLoopBlock(getNextName(),conditionExpr,stmt)
        if(loopNode.Invs != Nil){
          loop.setInvariant(
            loopNode.Invs
              .map(translatePExpression(codeTranslator,_))
              .reduce(currentExpressionFactory.makePBinaryExpression(And()(loopNode),_,_,loopNode)))
        } else {
          loop.setInvariant(TrueExpression()(loopNode))
        }
        val loopTranslator = new LoopBodyTranslator(this,loop)
        loopTranslator.translateBody(loopTranslator.translateStatement(_,body))

        //integrate loop into CFG
        currentBlock.setGoto(loop,stmt)
        val nextBlock = basicBlocks(getNextName("after_while"))
        loop.setGoto(nextBlock,stmt)
        continueWith(nextBlock)
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
        def withRhsTranslation[T](block : PTerm => T) : T = rhs match {
          case newObj:chalice.NewRhs =>
            temporaries.using(referenceType){targetVar =>
              translateNew(codeTranslator,newObj,targetVar)
              block(currentExpressionFactory.makeProgramVariableTerm(targetVar,newObj))
            }
          case expr:chalice.Expression =>
            val t = translatePTerm(codeTranslator, expr)
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
              val rcvrTerm = translatePTerm(codeTranslator, rcvr)
              currentBlock.appendAssignment(rcvrVar,rcvrTerm,rcvr)
              withRhsTranslation(currentBlock.appendFieldAssignment(rcvrVar,fields(location.f),_,stmt))
            }
        }
      case callNode:chalice.CallAsync => translateMethodFork(callNode)
      case c:chalice.Call if c.m.isInstanceOf[chalice.Method] => translateMethodCall(c)
      case callNode:chalice.JoinAsync => translateMethodJoin(callNode)
      case foldNode:chalice.Fold => translateFold(codeTranslator, foldNode)
      case unfoldNode:chalice.Unfold => translateUnfold(codeTranslator,unfoldNode)
      case otherStmt => report(messages.UnknownAstNode(otherStmt))
    }

    assert(blockStack.length == oldStackSize,"translate(chalice.Statement) changed the size of the blockStack when translating %s. Expected %d, actual %d"
      .format(stmt,oldStackSize,blockStack.length))
    assert(blockStack.view.drop(1).sameElements(stackTail),"translate(chalice.Statement) changed the tail elements of the blockStack when translating %s."
      .format(stmt))
  }

  protected abstract sealed class ReadCondition

  protected final case class ReadImplication(lhs : PExpression, rhs : List[ReadCondition])
    extends ReadCondition
    with Product2[PExpression, List[ReadCondition]] {
    def _1 = lhs
    def _2 = rhs
  }
  protected final case class ReadField(reference : PTerm, field : FieldTranslator, permissionAmount : PTerm) extends ReadCondition

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
    case rf@ReadField(_,_,perm) if allowsInexactChecking(perm,methodFraction) == inexact => rf
    case ReadImplication(lhs,rs2) => val rs3 =
        filterCondByChecking(rs2, inexact, methodFraction).collect({
          case rf@ReadField(_,_,_) => rf
          case ri@ReadImplication(_,_::_) => ri //we're not interested in empty lists
        })
      ReadImplication(lhs, rs3)
  })

  /**
    * Produces SIL code that determines the read permission fraction (`k`) for a call (synchronous or asynchronous)
    * This method might introduce new SIL blocks into the CFG.
    * @param codeTranslator The translator to use for the receiver and the arguments.
    * @param callNode The Chalice AST node for the call. Either [[chalice.Call]] or [[chalice.CallAsync]].
    * @return the term that represents the chosen `k`.
    */
  def determineReadPermissionFraction(
                                       codeTranslator : CodeTranslator,
                                       callNode : chalice.Statement { def obj : chalice.Expression;  def args : List[chalice.Expression]; def m : chalice.Callable }) : PTerm = {
    val args = callNode.args;
    val calleeFactory = methods(callNode.m.asInstanceOf[chalice.Method])

    languageConstruct(callNode){ ctor =>
      import ctor._
    
      //Read (fractional) permissions
      val callReadFractionVariable = declareScopedVariable(callNode, getNextName("k"), permissionType) // a unique variable for every method invocation
      val callReadFractionTerm : PTerm = callReadFractionVariable
      // `inhale 0 < k ∧ k < full ∧ k < method_k`
      val kPositive = permissionLT.apply(noPermission,callReadFractionTerm)
      val kOnlyRead = permissionLT.apply(callReadFractionTerm,fullPermission)
      val kSubfraction = permissionLT.apply(callReadFractionTerm,this.environmentReadFractionTerm(callNode))
      inhale(List(kPositive,kOnlyRead,kSubfraction))
  
      // Permission maps
      // `var m_0 : Map[(ref,int),Permission]`
      val originalPermMapVar = declareScopedVariable(callNode,getNextName("m0"),prelude.Map.PermissionMap.dataType)
      val originalPermMapTerm = originalPermMapVar : PTerm
      // `var m : Map[(ref,int),Permission]`
      val permMapVar = declareScopedVariable(callNode,getNextName("m"),prelude.Map.PermissionMap.dataType)
      val permMapTerm = permMapVar : PTerm
      // `inhale m = m_0`
      inhale(permMapTerm === originalPermMapTerm)
  
      // Translate arguments and create mapping from parameter variables to these terms
      val argTerms =
        translatePTerm (codeTranslator, callNode.obj) ::
          args.map(translatePTerm(codeTranslator,_)) ++
            List(callReadFractionTerm)
  
      val callSubstitution = currentExpressionFactory.makePProgramVariableSubstitution(calleeFactory.parameters.zip(argTerms).map(x => x._1 -> x._2).toSet)
      def transplantExpression(e : PExpression) : PExpression = {
        e.substitute(callSubstitution)
      }
      def transplantPTerm(t : PTerm) : PTerm = {
        t.substitute(callSubstitution)
      }
  
      // Generate assumptions and conditions on fraction
      /**
        * Walks over an [[silAST.expressions.Expression]] and extracts just read permission assertions and implications.
        * @param expr The expression to analyse.
        * @return A list of extracted read conditions.
        */
      def genReadCond(expr : Expression) : List[ReadCondition] = expr match {
        case PermissionExpression(_,FullPermissionTerm()) => Nil
        case PermissionExpression(_,NoPermissionTerm()) => Nil
        case p@PermissionExpression(FieldLocation(reference:PTerm,field),pTerm:PTerm) =>
          List(ReadField(transplantPTerm(reference),fields.lookup(field.name),transplantPTerm(pTerm)))
        case PermissionExpression(nonReferenceTerm,_) =>
          report(messages.ContractNotUnderstood(expr))
          Nil
        case BinaryExpression(Implication(),lhs:PExpression,rhs) =>
          //use lhs as-is in implications
          List(ReadImplication(transplantExpression(lhs),genReadCond(rhs)))
        case BinaryExpression(Implication(),_,_) =>
          report(messages.PermissionNotUnderstood(callNode,expr))
          Nil
        case BinaryExpression(And(),lhs,rhs) =>
          List(lhs,rhs).map(genReadCond).flatten
        case BinaryExpression(Or(),lhs,rhs) =>
          report(messages.PermissionNotUnderstood(callNode,expr))
          Nil
        case BinaryExpression(Equivalence(),lhs,rhs) =>
          // Interpret A ↔ B ≡ (A → B) ∧ (B → A)
          genReadCond(conjunction(List(
            Implication()(callNode).t(lhs,rhs),
            Implication()(callNode).t(rhs,lhs)
          )))
        case  _:EqualityExpression
            | _:AtomicExpression
            | _:UnaryExpression => Nil
        case _ =>
          report(messages.PermissionNotUnderstood(callNode,expr))
          Nil
      }
  
      /**
        * Takes a list of read permission conditions as extracted by {{genReadCon}} and generates
        * the corresponding conditions on `k`.
        * @param rs the list of read permission conditions to implement.
        */
      def appendCond(rs : List[ReadCondition]){
        val combined = new CombinedPrecondition(this,this.environmentReadFractionTerm(callNode))
        rs foreach {
          case ReadField(reference,field,perm) =>
            val originalPermMapTerm = currentExpressionFactory.makeProgramVariableTerm(originalPermMapVar, callNode)
            val permMapTerm = currentExpressionFactory.makeProgramVariableTerm(permMapVar, callNode)
  
            val currentActualPermission = currentExpressionFactory.makePermTerm(reference,field)(callNode)
            temporaries.using(prelude.Pair.Location.dataType){ locationVar =>
              val location = currentExpressionFactory.makeProgramVariableTerm(locationVar, callNode)
  
              // Assert the precondition of the reference.
              combined.visitTerm(reference,null) match {
                case TrueExpression() => // `exhale true` only confuses
                case definedness => exhale(removeSideEffects(definedness))
              }
              
              // `location := (ref,field)`
              locationVar <-- field.locationLiteral(currentExpressionFactory, reference.asInstanceOf[PTerm])
  
              // `inhale  get(m_0,(ref,field)) = perm(ref,field)` where (ref,field) = location
              inhale((prelude.Map.PermissionMap.get.apply(originalPermMapTerm,location))===(currentActualPermission))

              val currentVirtualPermission = prelude.Map.PermissionMap.get.p(permMapTerm,location)

              // `exhale 0 < get(m,(ref,field))`
              exhale(permissionLT.apply(noPermission,currentVirtualPermission), "Permission to " + field.field + " might not be positive.")

              // `inhale k < get(m,(ref,field))`
              inhale(permissionLT.apply(perm,currentVirtualPermission))
  
              // `m := set(m,(ref,field),get(m,(ref,field)) - perm)`
              val nextVirtualPermission = permissionSubtraction.p(currentVirtualPermission,perm)
              permMapVar <-- (prelude.Map.PermissionMap.update.p(permMapTerm,location,nextVirtualPermission))
            }
          case _ =>
        }
  
        rs collect { case a@ReadImplication(_,_) => a } groupBy (_.lhs) foreach { i =>
          silIf(i._1){
            appendCond(i._2.map(_.rhs).flatten)
          } end()
        }
      }
  
      val readConds = calleeFactory.methodFactory.method.signature.precondition
        .map(genReadCond _)

      def emitConditionCode(allowInexact : Boolean) {
        appendCond(filterCondByChecking(readConds.flatten.toList,allowInexact,callReadFractionTerm))
      }

      // First handle all cases where we have some flexibility when choosing k,
      // and only then handle the cases where k is treated as fixed.
      emitConditionCode(allowInexact = true)
  
      callReadFractionTerm
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

    val readFractionTerm = determineReadPermissionFraction(codeTranslator,callNode)

    // Generate call statement
    val receiverTerm = translatePTerm(codeTranslator,receiver)
    val destinationVars = destinations.map(vExpr => programVariables(vExpr.v))
    val argTerms = args.map(translatePTerm(codeTranslator, _)) ++ List(readFractionTerm)
    currentBlock.appendCall(
      currentBlock.makeProgramVariableSequence(destinationVars, callNode),
      receiverTerm,
      calleeFactory,
      PTermSequence(argTerms : _*),callNode)
  }

  def translateMethodFork(callNode : chalice.CallAsync) {
    val codeTranslator = new MemberCodeTranslator
    val chalice.CallAsync(_,chaliceTokenVariable,receiver,_,args) = callNode
    val calleeFactory = methods(callNode.m)

    languageConstruct(callNode){ ctor =>
      import ctor._

      // `token := new object`
      val tokenVar = programVariables(chaliceTokenVariable.v)
      val token : ProgramVariableTerm = tokenVar
      tokenVar <-- NewRef()

      // `inhale acc(token.joinable,write)`
      inhale(acc(token,prelude.Token.joinable,fullPermission))

      // `token.joinable := true`
      (tokenVar!prelude.Token.joinable) <-- booleanTrue.p()

      //Determine read fraction
      val readFractionTerm = determineReadPermissionFraction(codeTranslator, callNode)

      // Store state (arguments)
      val rcvrTerm = translatePTerm(codeTranslator, receiver)
      val argTerms = rcvrTerm :: args.map(translatePTerm(codeTranslator,_)) ++ List(readFractionTerm)

      argTerms.zip(calleeFactory.callToken.args) foreach { a =>
        // `inhale acc(token.field,full);`
        inhale(acc(token,a._2,fullPermission))
        // `token.field := arg`
        (tokenVar!a._2) <-- a._1
      }

      //Store state (old(*))
      val callSiteSubstitution = currentExpressionFactory.makePProgramVariableSubstitution(calleeFactory.parameters.zip(argTerms).map(x => x._1 -> x._2).toSet)
      calleeFactory.callToken.oldTerms foreach { entry =>
        val oldNode = entry._1
        val tkField = entry._2
        val cp = new CombinedPrecondition(this,readFractionTerm)
        val choice = booleanEvaluate.p(declareScopedVariable(callNode,getNextName("nondeterministic_choice"),booleanType))

        // `inhale acc(tk.field,full);`
        inhale(acc(tokenVar,tkField,fullPermission))

        // `if(*) { inhale precondition(e); tk.field = e; }`
        //silIf(choice,callNode){
          oldNode match {
            case OldTermNode(OldTerm(inner:PTerm)) =>
              val innerLocal = inner.substitute(callSiteSubstitution)
              inhale(cp.visitTerm(innerLocal,null))
              (tokenVar!tkField) <-- innerLocal
            case OldExpressionNode(OldExpression(inner:PExpression)) =>
              val innerLocal = inner.substitute(callSiteSubstitution)
              inhale(cp.visitExpression(innerLocal,null))
              silIf(innerLocal,callNode){
                (tokenVar!tkField) <-- booleanTrue.p()
              } els {
                (tokenVar!tkField) <-- booleanFalse.p ()
              } end()
            case o => // inner term/expression is not a program term/expression
              report(messages.ContractNotUnderstood(o.astNode))
          }
        //} end ()
      }

      // Finally: `exhale precondition(method)`, with parameters substituted
      if(calleeFactory.method.signature.precondition.size > 0)
        exhale(calleeFactory.method.signature.precondition
          .map(_.substitute(callSiteSubstitution)))
    }
  }
  
  def translateMethodJoin(callNode : chalice.JoinAsync) {
    val codeTranslator = new MemberCodeTranslator()
    languageConstruct(callNode){ ctor =>
      import ctor._

      val (tokenVar,allocatedTemp) = translatePTerm(codeTranslator, callNode.token) match {
        case ProgramVariableTerm(v) => (v,false)
      case t =>
        val v = temporaries.acquire(referenceType)
        v <-- t
        (v,true)
    }

      val tokenTerm : PTerm = tokenVar
      val calleeFactory = methods(callNode.m)
      val tokenStorage = calleeFactory.callToken
      val resultTargets = callNode.lhs.map(ve => programVariables(ve.v))

      // `exhale eval(token.joinable)`
      exhale(booleanEvaluate.apply(tokenTerm!prelude.Token.joinable))

      // set up substitution
      val resultTerms = tokenStorage.results.map(tokenTerm!_)
      val argumentTerms = tokenStorage.args.map(tokenTerm!_)
      val sig = calleeFactory.methodFactory.method.signature
      val (parameterVariables,resultVariables) = (sig.parameters,sig.results)
      val joinSubstitution = currentExpressionFactory.makePProgramVariableSubstitution(
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
        val methodPostcondition = sig.postcondition
          .map(trans.transplant(_))
        val resultAccess = tokenStorage.results
          .map(acc(tokenTerm,_,fullPermission))
        inhale(resultAccess,methodPostcondition)

      // assign all result fields to result variables
      if(tokenStorage.results.size > 0){
        resultTargets.zip(resultTerms).foreach(t => t._1 <-- t._2)
      }

      // finally set `token.joinable := false`
      (tokenVar!prelude.Token.joinable) <-- booleanFalse.p()

      if(allocatedTemp){
        temporaries.release(tokenVar)
      }
    }
  }

  def removeSideEffects(expr : Expression) : Expression = {
    val remover = new ExpressionTransplantation(this) {
      def translateProgramVariable(variable : ProgramVariable) = 
        currentExpressionFactory.makeProgramVariableTerm(variable, variable.sourceLocation)

      override def transplant(expression : Expression) = expression match {
        case PermissionExpression(FieldLocation(ref,field),amount) =>
          // `amount ≤ perm(ref,field)`
          currentExpressionFactory.makeDomainPredicateExpression(
            permissionLE,TermSequence(amount,
              currentExpressionFactory.makePermTerm(ref,field)(expression)
          ),expression)
        case _ => super.transplant(expression)
      }
    }
    remover.transplant(expr)
  }

  def translateAssert(expr : chalice.Expression) {
    val translator = new MemberCodeTranslator with AssertionTranslator
    currentBlock.appendExhale(translator.translateExpression(expr),expr)
  }

  def translateAssume(expr : chalice.Expression) {
    val translator = new MemberCodeTranslator with AssertionTranslator
    currentBlock.appendInhale(translator.translateExpression(expr),expr)
  }

  protected def translateAssignment(codeTranslator : CodeTranslator, lhs : chalice.VariableExpr,  rhs : chalice.RValue){
    val targetVariable = programVariables(lhs.v)

    rhs match {
      //chalice.RValue is (expression ∪ new-obj)
      //NewRhs  is used for both object creation and channel creation (where lower and upper bounds come into play)
      case newObj@chalice.NewRhs(typeId,init,lowerBound,upperBound) =>
        translateNew(codeTranslator,newObj,targetVariable)
      case e:chalice.Expression =>
        val rhsTerm = translatePTerm(codeTranslator, e)
        currentBlock.appendAssignment(targetVariable,rhsTerm,lhs)
    }

    assert(currentBlock.programVariables contains targetVariable,
      "The SIL basic block %s is expected to have the SIL program variable %s in scope. Program variables actually in scope: {%s}"
        .format(currentBlock.name,targetVariable,currentBlock.programVariables.mkString(", ")))
  }

  protected def translateNew(codeTranslator : CodeTranslator, newObj : chalice.NewRhs, targetVar : ProgramVariable) {
    currentBlock.appendNew(targetVar,referenceType,newObj)
    val refTerm = currentExpressionFactory.makeProgramVariableTerm(targetVar,newObj)
    val fullAccess = currentExpressionFactory.makeFullPermission(newObj)
    newObj.typ.Fields foreach { cf =>
      val field = fields(cf)
      currentBlock.appendInhale(
        currentExpressionFactory.makeFieldPermissionExpression(refTerm,field,fullAccess,newObj),newObj)
    }
    newObj.initialization foreach  { init =>
      val rhsTerm = translatePTerm(codeTranslator,init.e)
      currentBlock.appendFieldAssignment(targetVar,fields(init.f),rhsTerm,init)
    }
  }

  def translatePTerm(codeTranslator : CodeTranslator, expr : chalice.Expression) : PTerm = {
    codeTranslator.translateTerm(expr) match {
      case pt:PTerm => pt
      case t =>
        assert(false,"Expected program term. Actual type: %s. Location: %s.".format(t.getClass,t.sourceLocation))
        null
    }
  }

  def translatePExpression(codeTranslator : CodeTranslator, expr : chalice.Expression) : PExpression = {
    codeTranslator.translateExpression(expr) match {
      case pe:PExpression => pe
      case t =>
        assert(false,"Expected program expression. Actual type: %s. Location: %s.".format(t.getClass,t.sourceLocation))
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
    currentBlock.appendUnfold(location,predicates(predicateAccess.ma.predicate),codeTranslator.translatePermission(predicateAccess.perm),unfoldNode)
  }

  class MemberCodeTranslator extends DefaultCodeTranslator(thisScopeTranslator) {
    override protected def readFraction(location : SourceLocation) = environmentReadFractionTerm(location)
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////      HELPER FUNCTIONS (TRANSLATION DSL)                              /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected def silIf[T](cond : PExpression, conditionLocation : SourceLocation = noLocation)(thnBlock : => T) = new {
    def els[U](elsBlock : => U) = new {
      def end() = {
        translateSilCondition[T, U](cond,() => thnBlock,Some(() => elsBlock),Some(conditionLocation))
      }
    }
    def end() = {
      translateSilCondition[T,Unit](cond,() => thnBlock,None,Some(conditionLocation))
    }
  }

  def translateSilCondition[T,U](
                                  condExpr : PExpression,
                                  thn : () => T,
                                  elsOpt : Option[() => U] = None,
                                  thnLoc : Option[SourceLocation] = None,
                                  elsLoc : Option[SourceLocation] = None) : (T, Option[U]) = {

    val thenLocation = thnLoc.getOrElse(noLocation)
    val elseLocation = elsLoc.getOrElse(noLocation)

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
    protected def construct(key : String) = cfgFactory.addBasicBlock(key,noLocation)
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
