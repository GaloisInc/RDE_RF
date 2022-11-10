package DocumentEnrichers

import Formatter.InlineFormatter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.File


class FRETDocumentEnricherTest extends AnyFlatSpec with should.Matchers {

  val fretFileReq = getClass.getResource("../Fret/requirements.json").getPath
  val fretFileRTSReq = getClass.getResource("../Fret/RTS_Requirements.json").getPath
  val formatter = new InlineFormatter()

  "FRETDocumentEnricher" should "parse a fret document" in {
    val jsonString =
      """
        |[
        |  {
        |        "reqid": "INSTRUMENTATION_SET_MANUAL_TRIP_TEMPERATURE",
        |        "parent_reqid": "INSTRUMENTATION_TRIP_TEMPERATURE",
        |        "project": "HARDENS",
        |        "rationale": "RFP [10]",
        |        "comments": "",
        |        "fulltext": "Upon MAINTENANCE & TEMPERATURE_MODE = 2 Instrumentation shall, until MAINTENANCE & !(TEMPERATURE_MODE = 2), satisfy TRIP_TEMPERATURE",
        |        "semantics": {
        |            "type": "nasa",
        |            "scope": {
        |                "type": "null"
        |            },
        |            "condition": "regular",
        |            "timing": "until",
        |            "response": "satisfaction",
        |            "variables": [
        |                "MAINTENANCE",
        |                "TEMPERATURE_MODE",
        |                "TRIP_TEMPERATURE"
        |            ],
        |            "qualifier_word": "upon",
        |            "pre_condition": "(MAINTENANCE & TEMPERATURE_MODE = 2)",
        |            "regular_condition": "(MAINTENANCE & TEMPERATURE_MODE = 2)",
        |            "conditionTextRange": [
        |                0,
        |                38
        |            ],
        |            "component_name": "Instrumentation",
        |            "componentTextRange": [
        |                40,
        |                54
        |            ],
        |            "stop_condition": "(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))",
        |            "timingTextRange": [
        |                61,
        |                106
        |            ],
        |            "post_condition": "(TRIP_TEMPERATURE)",
        |            "responseTextRange": [
        |                108,
        |                131
        |            ],
        |            "ft": "((LAST V (((! <b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b>) & ((! LAST) & (X <b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b>))) -> (X ((<b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b> V (<b><i>(TRIP_TEMPERATURE)</i></b> | <b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b>)) | (LAST V <b><i>(TRIP_TEMPERATURE)</i></b>))))) & (<b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b> -> ((<b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b> V (<b><i>(TRIP_TEMPERATURE)</i></b> | <b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b>)) | (LAST V <b><i>(TRIP_TEMPERATURE)</i></b>))))",
        |            "pt": "(H ((H (! <b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b>)) | (((! <b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b>) S ((! <b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b>) & (<b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b> & ((Y (! <b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b>)) | FTP)))) -> <b><i>(TRIP_TEMPERATURE)</i></b>)))",
        |            "ftExpanded": "((LAST V (((! <b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b>) & ((! LAST) & (X <b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b>))) -> (X ((<b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b> V (<b><i>(TRIP_TEMPERATURE)</i></b> | <b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b>)) | (LAST V <b><i>(TRIP_TEMPERATURE)</i></b>))))) & (<b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b> -> ((<b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b> V (<b><i>(TRIP_TEMPERATURE)</i></b> | <b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b>)) | (LAST V <b><i>(TRIP_TEMPERATURE)</i></b>))))",
        |            "ptExpanded": "(H ((H (! <b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b>)) | (((! <b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b>) S ((! <b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b>) & (<b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b> & ((Y (! <b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b>)) | (! (Y TRUE)))))) -> <b><i>(TRIP_TEMPERATURE)</i></b>)))",
        |            "component": "<b><i>Instrumentation</i></b>",
        |            "CoCoSpecCode": "(H((H( not (MAINTENANCE and TEMPERATURE_MODE = 2))) or ((SI( ((MAINTENANCE and TEMPERATURE_MODE = 2) and ((pre ( not (MAINTENANCE and TEMPERATURE_MODE = 2))) or FTP)), ( not (MAINTENANCE and not ( TEMPERATURE_MODE = 2 ))) )) => (TRIP_TEMPERATURE))))",
        |            "diagramVariables": "TC = <b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b>, SC = <b><i>(MAINTENANCE & ! ( TEMPERATURE_MODE = 2 ))</i></b>, Response = <b><i>(TRIP_TEMPERATURE)</i></b>.",
        |            "description": "ENFORCED: in the interval defined by the entire execution.\nTRIGGER: first point in the interval if <b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b> is true and any point in the interval where <b><i>(MAINTENANCE & TEMPERATURE_MODE = 2)</i></b> becomes true (from false).\nREQUIRES: for every trigger, RES must remain true until (but not necessarily including) the point where the stop condition holds, or to the end of the interval. If the stop condition never occurs, RES must hold until the end of the scope, or forever.  If the stop condition holds at the trigger, the requirement is satisfied.",
        |            "diagram": "_media/user-interface/examples/svgDiagrams/null_regular_until_satisfaction.svg"
        |        },
        |        "_id": "30165290-8ad1-11ec-891b-e36dd42a8cc0"
        |    }
        |]
        |""".stripMargin
    val fretDocument = FretJsonMethods.parseJsonToFretDocument(jsonString)
    fretDocument should not be null
    fretDocument.size should be (1)
  }


  "FRETDocumentEnricher" should "parse a fret file" in {
    val fretDocumentEnricher = new FRETDocumentEnricher()
    val inputFile = fretFileReq
    val fretDocument = fretDocumentEnricher.parseDocument(inputFile)
    fretDocument should not be null

    val outputFile = new File(fretFileReq).getParentFile.getAbsolutePath + "/fret_requirements.json"
    fretDocumentEnricher.createDecoratedFile(fretDocument, outputFile, formatter)

  }

  "FRETDocumentEnricher" should "parse both of the FRET files" in {
    val fretDocumentEnricher = new FRETDocumentEnricher()
    val inputFileReq = fretFileReq
    val fretDocumentReq = fretDocumentEnricher.parseDocument(inputFileReq)
    fretDocumentReq should not be null
    val outputFileReq = new File(fretFileReq).getParentFile.getAbsolutePath + "/fret_requirements.json"
    fretDocumentEnricher.createDecoratedFile(fretDocumentReq, outputFileReq, formatter)

    val inputFileRTSReq = fretFileRTSReq
    val fretDocumentRTSReq = fretDocumentEnricher.parseDocument(inputFileRTSReq)
    fretDocumentRTSReq should not be null
    val outputFileRTSReq = new File(fretFileRTSReq).getParentFile.getAbsolutePath + "/fret_requirements_vars_spec.json"
    fretDocumentEnricher.createDecoratedFile(fretDocumentRTSReq, outputFileRTSReq, formatter)
  }
}


