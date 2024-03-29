package ConfigParser

import com.typesafe.config.ConfigFactory
import org.apache.logging.log4j.scala.Logging
import pureconfig.ConfigReader.Result
import pureconfig.ConfigSource
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._

import java.io.{File, InputStream, InputStreamReader}
import java.nio.file.{Files, Paths}

object RefinementLoader extends Logging {
  def load(file: String): MasterModel = {
    if (!Files.exists(Paths.get(file))) {
      val msg = f"File not found: $file. Current working directory is: ${System.getProperty("user.dir")}."
      logger.error(msg)
      throw new IllegalArgumentException(msg)
    }
    logger.info(s"Loading Refinement config from $file")
    val conf = ConfigFactory.parseFile(new File(file))
    val parsingResults = ConfigSource.fromConfig(conf).load[RefinementFileConfig]
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
      implicit val hintMasterConfig: ProductHint[RefinementFileConfig] = ProductHint[RefinementFileConfig](allowUnknownKeys = false)

      val parsingResults = ConfigSource.fromConfig(conf).load[RefinementFileConfig]
      val masterConfig = extractMasterConfig(parsingResults)
      parse(masterConfig)
    } finally {
      reader.close()
    }
  }


  def extractMasterConfig(parsingResults: Result[RefinementFileConfig]): RefinementFileConfig = {
    parsingResults match {
      case Left(errors) =>
        logger.error("Errors during parsing.")
        for (e <- errors.toList) {
          //prettyPrintError(e)
        }
        throw new IllegalArgumentException(errors.toString())
      case Right(master) =>
        logger.info(f"Successfully parsed master configuration.")
        master
    }
  }

  def parse(config: RefinementFileConfig): MasterModel = {
    config match {
      case RefinementFileConfig(name, implicit_refinements, explicit_refinements) =>
        val implicit_refinementModels = implicit_refinements.map(i => i._1 -> i._2.map(parseRefinement))
        val explicit_refinementModels = explicit_refinements.map(e => e._1 -> e._2.map(parseRefinement))
        MasterModel(name, implicit_refinementModels, explicit_refinementModels)
      case _ =>
        logger.error("Could not parse config.")
        throw new IllegalArgumentException("Invalid config")
    }
  }

  private def parseRefinement(refinement: String): RefinementModel = {
    RefinementParserSingleton.parse(RefinementParserSingleton.refinement, refinement) match {
      case RefinementParserSingleton.Success(result, _) =>
        RefinementModel(srcRef = result.srcRef, trgRef = result.trgRef)
      case RefinementParserSingleton.NoSuccess(msg, _) =>
        logger.error(msg)
        throw new IllegalArgumentException(msg)
      case _ =>
        logger.error("Unknown error during parsing.")
        throw new IllegalArgumentException("Invalid refinement")
    }
  }
}




