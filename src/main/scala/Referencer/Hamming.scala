package Referencer

import java.util.Locale

object Hamming {
  def compute(s1: String, s2: String): Int = {
    //Ensure that both strings have the same length
    require(s1.length == s2.length, "Strings must have the same length")
    s1.zip(s2).count(pair => pair._1.toLower != pair._2.toLower)
  } ensuring ((res: Int) => res <= math.max(s1.length, s2.length) && res >= 0, "Result must be between 0 and the length of the longest string")

  def computeRelHamming(s1: String, s2: String): Double = {
    require(s1.nonEmpty && s2.nonEmpty, "Strings must not be empty")
    val cleanedString1 = s1.toLowerCase(Locale.US).trim()
    val cleanedString2 = s2.toLowerCase(Locale.US).trim()

    val paddedString1 = cleanedString1.padTo(cleanedString2.length, ' ')
    val paddedString2 = cleanedString2.padTo(paddedString1.length, ' ')

    assert(paddedString1.length == paddedString2.length, "Strings are not of equal length")

    val hammingDistance = compute(paddedString1, paddedString2)
    //Both strings have the same length so the choice does not matter
    val length = paddedString1.length
    hammingDistance.toDouble / length.toDouble
  } ensuring ((res: Double) => res <= 1.0 && res >= 0.0)

}
