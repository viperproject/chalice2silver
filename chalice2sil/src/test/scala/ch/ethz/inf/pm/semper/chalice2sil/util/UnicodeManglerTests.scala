package ch.ethz.inf.pm.semper.chalice2sil.utill

import ch.ethz.inf.pm.semper.chalice2sil.ChaliceSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import silAST.programs.symbols.ProgramVariable
import org.scalatest.matchers.{MatchResult, BeMatcher, ShouldMatchers}
import org.scalatest.FunSuite
import ch.ethz.inf.pm.semper.chalice2sil.translation.util.UnicodeMangler

/**
  * @author Christian Klauser
  */
@RunWith(classOf[JUnitRunner])
class UnicodeManglerTests extends FunSuite with ShouldMatchers {

  class AsciiMatcher extends BeMatcher[String] {
    def apply(left : String) = MatchResult(left.forall(_.toInt < 128),
      "\"%s\" contains non-ASCII characters.".format(left),
      "\"%s\" does not contain non-ASCII characters.".format(left))
  }

  val ascii = new AsciiMatcher

  def roundtrip(source : String, escapeChar : Char = '_') {
    test(escapeChar.toString + ":" + source) {
      val mangler = new UnicodeMangler(escapeChar)
      println("Source: \"" + source + "\", escapeChar='" + escapeChar + "'")
      val a = mangler.mangle(source)
      println("Mangled: \"" + a + "\"")
      a should be(ascii)
      val u = mangler.unmangle(a)
      println("Unmangled: \"" + u + "\"")
      u should be === source
    }
  }

  roundtrip("")
  roundtrip("_")
  roundtrip("__")
  roundtrip("___")
  roundtrip("____")
  roundtrip("_____")
  roundtrip("______")
  roundtrip("fλf")
  roundtrip("f_λ_f")
  roundtrip("f__λ__f")
  roundtrip("_03BB")
  roundtrip("λλ")
  roundtrip("αβγδεη")
  roundtrip("Γ_a")
}
