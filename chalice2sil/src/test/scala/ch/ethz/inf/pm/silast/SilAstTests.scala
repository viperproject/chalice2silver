package ch.ethz.inf.pm.silast

import org.scalatest.FunSuite
import silAST.programs.{ProgramFactory, Program}
import silAST.source.noLocation
import silAST.types.{DataTypeSequence, integerType, referenceType}
import org.scalatest.matchers.ShouldMatchers
import silAST.domains.DomainFunction

/**
  * @author Christian Klauser
  */
class SilAstTests extends FunSuite with ShouldMatchers {
  test("two-identical-assignments"){
    val p = new ProgramFactory("two-identical-assignments")(noLocation,Nil)
    val mf = p.getMethodFactory("main")(noLocation)
    val imf = mf.addImplementation(noLocation)
    val x = imf.addProgramVariable("x",integerType)(noLocation)
    val cfg = imf.cfgFactory
    val block = cfg.addBasicBlock("entry",noLocation)

    //perfectly valid program, if a little non-sensical
    // but it would be easy enough to create a useful example
    block.appendAssignment(x,block.makeIntegerLiteralTerm(5,noLocation),noLocation)
    block.appendAssignment(x,block.makeIntegerLiteralTerm(5,noLocation),noLocation)
  }

  test("Non-parametrised domain with functions") {
    val pf = new ProgramFactory("two-identical-assignments")(noLocation,Nil)
    val factory = pf.getDomainFactory("Glob",Nil,noLocation)
    val constant = factory.defineDomainFunction("someConstant",DataTypeSequence(),integerType,noLocation)
    val p = pf.getProgram

    constant.domain.functions should contain (constant : DomainFunction)
    val domainOpt = p.domains.find(_.name.startsWith(constant.domain.name))
    domainOpt should not be ('empty)
    domainOpt.get.functions should not be 'empty
    domainOpt.get.functions.find(_.name == constant.name) should not be 'empty
  }
}
