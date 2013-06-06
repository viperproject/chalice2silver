package semper.chalice2sil.util

import org.junit.runner.RunWith
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


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
  roundtrip("_alphaα")
  roundtrip("_α")

  roundtrip("",'\\')
  roundtrip("\\",'\\')
  roundtrip("\\\\",'\\')
  roundtrip("\\\\\\",'\\')
  roundtrip("fλf",'\\')
  roundtrip("f_λ\\f",'\\')
  roundtrip("f\\_λ__f",'\\')
  roundtrip("\\03BB",'\\')
  roundtrip("λλ",'\\')
  roundtrip("αβγδεη",'\\')
  roundtrip("Γ\\a",'\\')
  roundtrip("\\alpha",'\\')
}
