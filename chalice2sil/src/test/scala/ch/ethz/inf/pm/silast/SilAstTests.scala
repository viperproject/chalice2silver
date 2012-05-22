package ch.ethz.inf.pm.silast

import org.scalatest.FunSuite
import silAST.programs.{ProgramFactory, Program}
import silAST.source.noLocation
import silAST.types.{integerType, referenceType}

/**
  * @author Christian Klauser
  */
class SilAstTests extends FunSuite {
  test("two-identical-assignments"){
    val p = new ProgramFactory("two-identical-assignments")(noLocation)
    val mf = p.getMethodFactory("main")(noLocation)
    val imf = mf.addImplementation()(noLocation)
    val x = imf.addProgramVariable("x",integerType)(noLocation)
    val cfg = imf.cfgFactory
    val block = cfg.addBasicBlock("entry")(noLocation)

    //perfectly valid program, if a little non-sensical
    // but it would be easy enough to create a useful example
    block.appendAssignment(x,block.makeIntegerLiteralTerm(5)(noLocation))(noLocation)
    block.appendAssignment(x,block.makeIntegerLiteralTerm(5)(noLocation))(noLocation)
  }
}
