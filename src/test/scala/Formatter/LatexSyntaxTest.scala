package Formatter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Types.LatexReferenceType

class LatexSyntaxTest extends AnyFlatSpec with should.Matchers {

  "LatexSyntax" should "add label" in {
    val labelText = "reference1"
    val label = LatexSyntax.addLabel(labelText)
    label should be(s"\\label{$labelText}")
  }

  "LatexSyntax" should "add clever reference" in {
    val labelText = "reference1"
    val label = LatexSyntax.addCref(labelText)
    label should be(s"\\cref{$labelText}")
  }

  "LatexSyntax" should "add varioref reference" in {
    val labelText = "reference1"
    val label = LatexSyntax.addVref(labelText)
    label should be(s"\\vref{$labelText}")
  }

  "LatexSyntax" should "add addClickable reference" in {
    val labelText = "reference1"
    val nameOfReference = "nameOfReference"
    val reference = LatexSyntax.addClickableLocalLink(labelText, nameOfReference, LatexReferenceType.Link)
    reference should be(s"\\link{$labelText}{$nameOfReference}")
  }

  "LatexSyntax" should "add addClickable reference and sanitizeName" in {
    val labelText = "reference1"
    val nameOfReference = "nameOf_Reference"
    val reference = LatexSyntax.addClickableLocalLink(labelText, nameOfReference, LatexReferenceType.File)
    val sanitizedName = LatexSanitizer.sanitizeName(nameOfReference)
    reference should be(s"\\fileLink{$labelText}{$sanitizedName}")
  }

}
