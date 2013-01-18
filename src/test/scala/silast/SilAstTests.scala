package ch.ethz.inf.pm.silast

import org.scalatest.FunSuite
import semper.sil.ast.programs.{ProgramFactory, Program}
import semper.sil.ast.source.NoLocation
import semper.sil.ast.types.{DataTypeSequence, integerType, referenceType}
import org.scalatest.matchers.ShouldMatchers
import semper.sil.ast.domains.DomainFunction
import semper.sil.ast.expressions.util.ExpressionSequence

/**
  * @author Christian Klauser
  */
class SilAstTests extends FunSuite with ShouldMatchers {
  test("two-identical-assignments"){
    val p = new ProgramFactory("two-identical-assignments")(NoLocation,Nil)
    val mf = p.getMethodFactory("main")(NoLocation)
    val imf = mf.addImplementation(NoLocation)
    val x = imf.addProgramVariable("x",integerType)(NoLocation)
    val cfg = imf.cfgFactory
    val block = cfg.addBasicBlock("entry",NoLocation)

    //perfectly valid program, if a little non-sensical
    // but it would be easy enough to create a useful example
    block.appendAssignment(x,block.makeIntegerLiteralExpression(5,NoLocation),NoLocation)
    block.appendAssignment(x,block.makeIntegerLiteralExpression(5,NoLocation),NoLocation)
  }

  test("Parametrised domain with functions") {
    val pf = new ProgramFactory("param-domain-func")(NoLocation,Nil)
    val factory = pf.getDomainFactory("Glob",List((NoLocation,"T",Nil)),NoLocation)
    val constant = factory.defineDomainFunction("someConstant",DataTypeSequence(),integerType,NoLocation)
    val inst = pf.makeDomainInstance(factory,DataTypeSequence(integerType))
    val p = pf.getProgram

    constant.domain.functions should contain (constant : DomainFunction)
    val domainOpt = p.domains.find(_.name.startsWith(inst.name))
    domainOpt should not be ('empty)
    domainOpt.get.functions should not be 'empty
    domainOpt.get.functions.find(_.name == constant.name) should not be 'empty

    pf.domainFunctions.find(_.name == constant.name) should not be 'empty
  }

  test("Non-parametrised domain with functions") {
    val pf = new ProgramFactory("non-param-domain-func")(NoLocation,Nil)
    val factory = pf.getDomainFactory("Glob",Nil,NoLocation)
    val constant = factory.defineDomainFunction("someConstant",DataTypeSequence(),integerType,NoLocation)
    val inst = pf.makeDomainInstance(factory,DataTypeSequence())
    factory.compile()
    val p = pf.getProgram

    constant.domain.functions should contain (constant : DomainFunction)
    val domainOpt = p.domains.find(_.name.startsWith(inst.name))
    domainOpt should not be ('empty)
    domainOpt.get.functions should not be 'empty
    domainOpt.get.functions.find(_.name == constant.name) should not be 'empty

    pf.domainFunctions.find(_.name == constant.name) should not be 'empty
  }

  test("Use function from unparameterised domain (lookup)") {
    val pf = new ProgramFactory("non-param-domain-func")(NoLocation,Nil)
    val factory = pf.getDomainFactory("Glob",Nil,NoLocation)
    val constant = factory.defineDomainFunction("someConstant",DataTypeSequence(),integerType,NoLocation)
    val inst = pf.makeDomainInstance(factory,DataTypeSequence())

    {
      val mf = pf.getMethodFactory("main")(NoLocation)
      val constant2 = inst.functions.find(_.name == constant.name).get
      mf.addPrecondition(mf.makeEqualityExpression(mf.makeIntegerLiteralExpression(0,NoLocation),
        mf.makeDomainFunctionApplicationExpression(constant2,ExpressionSequence(),NoLocation,List("works")),
        NoLocation),
        NoLocation)
    }

    val p = pf.getProgram

    p
  }
}
