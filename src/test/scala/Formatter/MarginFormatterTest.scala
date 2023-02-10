package Formatter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MarginFormatterTest extends AnyFlatSpec with should.Matchers {
  val marginFormatter = new MarginFormatter

  "MarginFormatter" should "put in margin note 1" in {
    val input = "https://www.google.com/search?q=scala+test+underscore"
    val expected =
      """\marginpar{
        |\begin{tiny}
        |https://www.google.com/search?q=scala+test+underscore
        |\end{tiny}
        |\par
        |}""".stripMargin
    marginFormatter.formatLatex(input) should be(expected)
  }

  "MarginFormatter" should "put in margin note 2" in {
    val input = "https://www.google.com"
    val expected =
      """\marginpar{
        |\begin{tiny}
        |https://www.google.com
        |\end{tiny}
        |\par
        |}""".stripMargin
    marginFormatter.formatLatex(input) should be(expected)
  }

  "MarginFormatter" should "put in margin note 3" in {
    val longTextString = "How are you doing \\textbf{today}?"
    val expected = """\marginpar{
                     |\begin{tiny}
                     |How are you doing \textbf{today}?
                     |\end{tiny}
                     |\par
                     |}""".stripMargin
    marginFormatter.formatLatex(longTextString) should be(expected)
  }
}
