package ch.ethz.inf.pm.semper.chalice2sil.translation

/**
  * Author: Christian Klauser
  */

import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil.{ChaliceSuite, Message, Program => _}
import silAST.programs.Program
import silAST.types.integerType
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class Basic extends ChaliceSuite with ShouldMatchers {  
  'locals.chalice {
    program.methods should have size (2)
    val mainOpt = program.methods.find(_.name == "Main::main")
    mainOpt should be ('defined)
    mainOpt.get.implementations should have size (1)
    val locals = mainOpt.get.implementations.head.locals
    
    locals should have size (2)
    locals.foreach(_.dataType should be (integerType))
  }
}
