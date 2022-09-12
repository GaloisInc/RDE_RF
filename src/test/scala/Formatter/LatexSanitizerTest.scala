package Formatter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LatexSanitizerTest extends AnyFlatSpec with should.Matchers {
  "LatexSanitizer" should "be able to sanitize underscored weblink" in {
    LatexSanitizer.sanitizeWebLink("https://www.dr.dk/test_underscore") should be ("https://www.dr.dk/test\\_underscore")
  }

  "LatexSanitizer" should "do nothing to simple link" in {
    LatexSanitizer.sanitizeWebLink("https://www.dr.dk/") should be("https://www.dr.dk/")
  }

  "LatexSanitizer" should "do nothing to simple link with text" in {
    LatexSanitizer.sanitizeWebLink("https://softwarefoundations.cis.upenn.edu/lf-current/Basics.html#lab44") should be("https://softwarefoundations.cis.upenn.edu/lf-current/Basics.html#lab44")
  }

  "LatexSanitizer" should "be able to remove special characters and spaces" in {
    LatexSanitizer.sanitizeReferenceName("Digital Instrumentation \\& Control") should be("Digital_Instrumentation___Control")
  }

  "LatexSanitizer" should "be able to escape underscores in links" in {
    LatexSanitizer.sanitizeName("Digital_Instrumentation_Control") should be("Digital\\_Instrumentation\\_Control")
  }

}
