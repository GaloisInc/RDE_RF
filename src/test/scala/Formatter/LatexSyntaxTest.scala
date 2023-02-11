package Formatter

import Report.{ClickableLink, Label, LatexReference}
import Types.LatexReferenceTypes
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LatexSyntaxTest extends AnyFlatSpec with should.Matchers {
  "LatexSyntax" should "add label" in {
    val labelText = "reference1"
    val label = Label(labelText)
    label.toLatex should be(s"\\label{$labelText}")
  }

  "LatexSyntax" should "add clever reference" in {
    val labelText = "reference1"
    val label = LatexReference(labelText, "cref")
    label.toLatex should be(s"\\cref{$labelText}")
  }

  "LatexSyntax" should "add varioref reference" in {
    val labelText = "reference1"
    val label = LatexReference(labelText, "vref")
    label.toLatex should be(s"\\vref{$labelText}")
  }

  "LatexSyntax" should "add addClickable reference" in {
    val labelText = "reference1"
    val nameOfReference = "nameOfReference"
    val reference = ClickableLink(labelText, nameOfReference, LatexReferenceTypes.Link)
    reference.toLatex should be(s"\\link{$labelText}{$nameOfReference}")
  }

  "LatexSyntax" should "add addClickable reference and sanitizeName" in {
    val labelText = "reference1"
    val nameOfReference = "nameOf_Reference"
    val reference = ClickableLink(labelText, nameOfReference, LatexReferenceTypes.Link)
    val sanitizedName = LatexSanitizer.sanitizeName(nameOfReference)
    reference.toLatex should be(s"\\link{$labelText}{$sanitizedName}")
  }

  "LatexSyntax" should "add addClickable for Abstraction" in {
    val labelText = "reference1"
    val nameOfReference = "nameOf_Reference"
    val reference = ClickableLink(labelText, nameOfReference, LatexReferenceTypes.Abstraction)
    val sanitizedName = LatexSanitizer.sanitizeName(nameOfReference)
    reference.toLatex should be(s"\\abstractionLink{$labelText}{$sanitizedName}")
  }

  "LatexSyntax" should "add addClickable for Refinement" in {
    val labelText = "reference1"
    val nameOfReference = "nameOf_Reference"
    val reference = ClickableLink(labelText, nameOfReference, LatexReferenceTypes.Refinement)
    val sanitizedName = LatexSanitizer.sanitizeName(nameOfReference)
    reference.toLatex should be(s"\\refinementLink{$labelText}{$sanitizedName}")
  }
}
