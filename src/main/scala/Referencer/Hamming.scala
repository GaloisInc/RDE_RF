package Referencer

import java.util.Locale

object Hamming {
  def compute(s1: String, s2: String): Int = {
    //Ensure that both strings have the same length
    require(s1.length == s2.length)
    s1.zip(s2).count(pair => pair._1.toLower != pair._2.toLower)
  } ensuring ((res: Int) => res <= math.max(s1.length, s2.length) && res >= 0)

  def computeRelHamming(s1: String, s2: String): Double = {
    require(s1.nonEmpty, "s1 is empty")
    require(s2.nonEmpty, "s2 is empty")

    val cleanedString1 = s1.toLowerCase(Locale.US).strip()
    val cleanedString2 = s2.toLowerCase(Locale.US).strip()

    val paddedString1 = cleanedString1.padTo(cleanedString2.length, ' ')
    val paddedString2 = cleanedString2.padTo(paddedString1.length, ' ')

    val distance = compute(paddedString1, paddedString2)
    //Both strings have the same length so the choice does not matter
    val length = paddedString1.length
    distance.toDouble / length.toDouble
  } ensuring ((res: Double) => res <= 1.0 && res >= 0.0)

}
