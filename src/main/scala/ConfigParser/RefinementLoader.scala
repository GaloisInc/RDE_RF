package ConfigParser

import com.typesafe.config.ConfigFactory
import pureconfig.ConfigConvert.fromReaderAndWriter
import pureconfig.ConfigReader.Result
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.auto._

import java.io.{File, InputStream, InputStreamReader}
import java.nio.file.{Files, Paths}

object RefinementLoader {

  def load(file: String): MasterModel = {
    if (!Files.exists(Paths.get(file))) {
      val msg = f"File not found: ${file}. Current working directory is: ${System.getProperty("user.dir")}."
      throw new IllegalArgumentException(msg)
    }
    val conf = ConfigFactory.parseFile(new File(file))
    val parsingResults = ConfigSource.fromConfig(conf).load[RefinementConfig]
    val masterConfig = extractMasterConfig(parsingResults)
    parse(masterConfig)
  }


  def load(stream: InputStream): MasterModel = {
    val reader = new InputStreamReader(stream)
    try {
      // read from stream
      val conf = ConfigFactory.parseReader(reader)

      // This forces pure config to not tolerate unknown keys in the config file.
      // It gives errors when typos happen.
      // From https://pureconfig.github.io/docs/overriding-behavior-for-case-classes.html
      //implicit val hintMasterConfig = ProductHint[RefinementConfig](allowUnknownKeys = false)

      val parsingResults = ConfigSource.fromConfig(conf).load[RefinementConfig]
      val masterConfig = extractMasterConfig(parsingResults)
      parse(masterConfig)
    } finally {
      reader.close()
    }
  }


  def extractMasterConfig(parsingResults: Result[RefinementConfig]): RefinementConfig = {
    parsingResults match {
      case Left(errors) => {
        //logger.error("Errors during parsing.")
        for (e <- errors.toList) {
          //prettyPrintError(e)
        }
        throw new IllegalArgumentException(errors.toString())
      }
      case Right(master) => {
        //logger.info(f"Successfully parsed master configuration.")
        //PPrint.pprint(master, l = logger)
        master
      }
    }
  }

  def parse(config: RefinementConfig): MasterModel = {
    config match {
      case RefinementConfig(name, refinements) =>
        val refinementModels = refinements.map(parseRefinement)
        MasterModel(name, refinementModels)
      case _ => throw new IllegalArgumentException("Invalid config")
    }
  }

  def parseRefinement(refinement: String): RefinementModel = {
    RefinementParserSingleton.parse(RefinementParserSingleton.refinement, refinement) match {
      case RefinementParserSingleton.Success(result, _) =>
        RefinementModel(srcRef = result.srcRef, trgRef = result.trgRef)
      case RefinementParserSingleton.NoSuccess(msg, _) => {

        throw new IllegalArgumentException(msg)
      }
    }
  }
}




