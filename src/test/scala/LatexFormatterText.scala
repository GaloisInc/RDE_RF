import Formatter.InlineFormatter
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path
import scala.collection.mutable
import scala.io.Source

class LatexFormatterText extends AnyFlatSpec with Matchers {
  private val formatter = InlineFormatter()
  it should "detect no difference between empty strands" in {
    //Todo
  }
}

