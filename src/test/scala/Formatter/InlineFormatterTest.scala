package Formatter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class InlineFormatterTest extends AnyFlatSpec with should.Matchers {
  val inlineFormatter = new InlineFormatter

  //The test cases are pretty self-explanatory.
  // The first test case is a simple URL with a query string.
  // The second test case is a simple URL without a query string.
  // The third test case is a simple text string with a bold text.

  "InlineFormatter" should "do nothing 1" in {
    val input = "https://www.google.com/search?q=scala+test+underscore"
    inlineFormatter.formatLatex(input) should be(input)
  }

  "InlineFormatter" should "do nothing 2" in {
    val input = "https://www.google.com"
    inlineFormatter.formatLatex(input) should be(input)
  }

  "InlineFormatter" should "do nothing 3" in {
    val longTextString = "How are you doing \\textbf{today}?"
    inlineFormatter.formatLatex(longTextString) should be(longTextString)
  }
}

