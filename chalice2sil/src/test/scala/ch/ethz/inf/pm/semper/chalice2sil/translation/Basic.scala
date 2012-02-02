package ch.ethz.inf.pm.semper.chalice2sil.translation

/**
  * Author: Christian Klauser
  */

import org.junit._
import runner.RunWith
import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil.{Message, Program => _, Chalice2SilRunner}
import silAST.programs.Program
import silAST.types.integerType

@RunWith(classOf[Chalice2SilRunner])
class Basic {
  import org.junit.matchers.JUnitMatchers._
  import org.hamcrest.CoreMatchers._
  import org.junit.Assert.assertThat
  
  @Test
  def locals(program : Program, messages : Seq[Message]){
    assertThat(program.methods.size,is(2))
    assertThat(program.methods.find(_.name == "Main::main").get.implementations.head.locals.size,is(2))
    val locals = program.methods.find(_.name == "Main::main").get.implementations.head.locals
    assertThat(locals.forall(_.dataType == integerType),is(true))
  }
  
  @Test
  def simpleCondition(program : Program,  messages : Seq[Message]){}
}
