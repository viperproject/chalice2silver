package ch.ethz.inf.pm.semper.chalice2sil.translation.cfg

import ch.ethz.inf.pm.semper.chalice2sil._
import collection._
import translation._
import cfg._
import silAST.programs.symbols.ProgramVariable
import util.NameSequence


/**
  * Extracts chalice basic blocks [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.ChaliceBlock]] from a
  * chalice method, while also translating chalice local variable assignments into SSA form.
  * The result is a control flow graph with a hybrid of chalice statements and SSA assignments.
  * @author Christian Klauser
  */
class CfgSurvey(programEnvironment: ProgramEnvironment, nameSequence : NameSequence)
  extends DerivedProgramEnvironment(programEnvironment) {

  /**
    * Generates a new unique name, optionally prefixed with the supplied string to aid debugging.
    * @param prefix The prefix of the generated name. Optional.
    * @return a unique name that is guaranteed to start with the supplied prefix.
    */
  protected def getNextName(prefix : String = "") =
    if(prefix == "")
      nameSequence.nextName
    else
      prefix + "_" + nameSequence.nextName

  protected def createNamedBlock(name : String, parentScope : ChaliceBlock) : ChaliceBlock = {
    val b = new ChaliceBlock(name)
    b.variablesInScope ++= parentScope.variablesInScope
    b
  }
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////  TRANSLATE TO CONTROL FLOW GRAPH                                                  ///////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def translateControlFlow(method : chalice.Method) = {
    val entryBlock = new ChaliceBlock("entry")
    val exitBlock =  translateStatementList(entryBlock,method.body)
    new ControlFlowSketch(entryBlock, exitBlock, method.ins, method.outs)
  }

  /**
    * Translates a sequence of statements. Starts by appending to the supplied
    * [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.ChaliceBlock]] and then switched to whatever blocks are
    * returned by [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.CfgSurvey.translateStatement]].
    * @param head The block to start appending chalice statements to.
    * @param stmts The sequence of chalice statements to translate to extract basic blocks from.
    * @return The chalice block that the last statement ended in. This block does not yet have outgoing edges;
    * you are free to append more statements to it.
    */
  protected def translateStatementList(head : ChaliceBlock, stmts : Seq[chalice.Statement]) : ChaliceBlock =
    stmts.foldLeft(head)(translateStatement(_,_)).ensuring(_.successors.isEmpty)

  /**
    * Translates a single chalice statement into the CFG sketch. Statements that do not represent control flow,
    * are appended to the supplied [[ch.ethz.inf.pm.semper.chalice2sil.translation.cfg.ChaliceBlock]], while
    * statements like `if` and `while` are translated into the corresponding block sub-graphs.
    * @param head The block to append the statement to or from which to branch away.
    * @param stmt The statement to be translated.
    * @return the block that control flow ends up in after the statement is executed. This block is guaranteed not to
    * have any outgoing edges yet. You are therefore free to append additional statements to it.
    */
  protected def translateStatement(head : ChaliceBlock, stmt : chalice.Statement) : ChaliceBlock = stmt match {
    case chalice.BlockStmt(stmts) => 
      translateStatementList(head, stmts)
    case chalice.IfStmt(cond, thn, elsOpt) =>
      val instanceName = getNextName("if")
      val thnBlock = createNamedBlock(instanceName + "_then", head)
      val endThnBlock = translateStatement(thnBlock, thn)
      val endIfBlock = createNamedBlock(instanceName + "_endif", head)

      elsOpt match {
        case None =>
          //connect blocks
          head ? cond --> thnBlock
          head ?¬cond --> endIfBlock
          endThnBlock --> endIfBlock
        case Some(els) =>
          val elsBlock = createNamedBlock(instanceName + "_else", head)
          val endElsBlock = translateStatement(elsBlock, els)

          //connect blocks
          head ? cond --> thnBlock
          head ?¬cond --> elsBlock
          endThnBlock --> endIfBlock
          endElsBlock --> endIfBlock
      }

      endIfBlock
    case w@chalice.WhileStmt(cond,_,_,_,body) =>
      val instanceName = getNextName("while")
      val whileContinue = createNamedBlock(instanceName + "_continue", head)
      val beginBody = createNamedBlock(instanceName + "_begin", head)
      val endBody = translateStatement(beginBody, body)
      val endWhile = createNamedBlock(instanceName + "_end", head)

      head --> whileContinue
      whileContinue ? cond --> beginBody
      whileContinue ?¬cond --> endWhile
      endBody backedgeTo whileContinue

      endWhile
    case _ =>
      head.variablesInScope ++= stmt.Declares
      head.statements += stmt
      head
  }
}
