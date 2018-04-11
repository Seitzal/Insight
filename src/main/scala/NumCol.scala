package seitzal.scalastat

import scala.annotation.tailrec

/**
 * Represents a data column containing only numeric values (quantifiable on an interval or ratio scale).
 * Missing values are not currently supported.
 */
case class NumCol (values : Vector[Double]) extends Col {

  lazy val length = values.length

  lazy val asStrList = for(e <- values.toList) yield e.toString

  override def toString = {
    val withtrailingcomma = (for(e <- values) yield e.toString + ", ").mkString
    withtrailingcomma.substring(0, withtrailingcomma.length - 2)
  }

  /**
   * The sum of all values of the column.
   * @return The sum of the column.
   */
  lazy val sum = Helper.round(values.foldLeft(0.0)(_ + _))

  /**
   * The arithmetic mean of all values of the column.
   */
  lazy val avg = Helper.round(sum / values.length)

  
  private lazy val sortedvs = values.sorted

  /**
   * The median value of the column.
   */
  lazy val median = (length % 2) match {
    case 1 => sortedvs(length / 2)
    case 0 => (sortedvs(length / 2 - 1) + sortedvs(length / 2)) / 2
  }

  /**
   * A derived column containing the deviations of each element in the column from the arithmetic mean
   */
  lazy val devs = derive(_ - avg)

  /**
   * A derived column containing the squared deviations of each element in the column from the arithmetic mean
   */
  lazy val devsSquared = derive((x : Double) => math.pow(x - avg, 2))

  /**
   * The variance of the column.
   * (The variance is the arithmetic mean of the squared deviations of each element from the arithmetic mean of the column)
   */
  lazy val variance = Helper.round(devsSquared.avg)

  /**
   * The standard deviation of the column, defined as the square root of the column's variance
   */
  lazy val standardDev = Helper.round(math.sqrt(variance))

  /**
   * A derived column containing the relative frequencies corresponding to the absolute frequencies in the column.
   * This only generates meaningful data if the original column contained absolute frequencies.
   */
  lazy val relfreq = derive(_ / sum)

  /**
   * A derived column containing the cumulative relative frequencies corresponding to the absolute frequencies in the column.
   * Cumulation is done from the head down the tail of the value list, i.e. top-to-bottom when imagining a table.
   * This only generates meaningful data if the original column contained absolute frequencies.
   */
  lazy val crelfreq = {
    @tailrec def loop(remainder : List[Double], acc : Vector[Double], prev : Double) : Vector[Double] =
      if(remainder.isEmpty) acc
      else {
        val crf = remainder.head + prev
        loop(remainder.tail, acc :+ crf, crf)
      }
    new NumCol(loop(relfreq.values.toList, Vector[Double](), 0).map(Helper.round))
  }

  /**
   * A derived column containing the cumulative absolute frequencies corresponding to the absolute frequencies in the column.
   * Cumulation is done from the head down the tail of the value list, i.e. top-to-bottom when imagining a table.
   * This only generates meaningful data if the original column contained absolute frequencies.
   */
  lazy val cabsfreq = {
    @tailrec def loop(remainder : List[Double], acc : Vector[Double], prev : Double) : Vector[Double] =
      if(remainder.isEmpty) acc
      else {
        val crf = remainder.head + prev
        loop(remainder.tail, acc :+ crf, crf)
      }
    new NumCol(loop(values.toList, Vector[Double](), 0).map(Helper.round))
  }

  /**
   * Calculates the Gini concentration index for an unclassified data column.
   * The gini index measures the relative concentration of a frequency distribution.
   * It is defined as the area between the distribution's Lorenz curve and the main diagonal f(x) = x.
   */
  lazy val gini = {
    val numerator = 2 * (
      (for (i <- 0 until length) yield (i + 1) * values(i))
      .foldLeft(0.0)(_ + _)
    )
    val subtrahend = (length.toDouble + 1) / length.toDouble
    Helper.round(numerator / (sum * length) - subtrahend)
  }

  /**
   * Calculates the normalised Gini concentration index for an unclassified data column.
   * The result is guaranted to be in [0;1] even for very short / small-n columns.
   */
  lazy val ngini = Helper.round(gini * (length.toDouble / (length.toDouble - 1)))

  /**
   * Calculates the Gini concentration index for a classified data column, assuming equal class breadth and flat distribution within classes.
   * The gini index measures the relative concentration of a frequency distribution.
   * It is defined as the area between the distribution's Lorenz curve and the main diagonal f(x) = x.
   */
  lazy val cgini = {
    val hstar = 1 / length.toDouble
    Helper.round(((for (i <- 0 until length) yield {
      if(i == 0)
        hstar * crelfreq.values(i)
      else
        hstar * (crelfreq.values(i - 1) + crelfreq.values(i))
    }).foldLeft(0.0)(_ + _)) - 1)
  }

  lazy val info = (
    "\n"
    + "n  = " + length.toString        + "\t\t"
    + "Σ  = " + sum.toString           + "\n"
    + "A  = " + avg.toString           + "\t"
    + "Md = " + median.toString        + "\n"
    + "S² = " + variance.toString      + "\t"
    + "S  = " + standardDev.toString   + "\n"
    + "G  = " + gini.toString          + "\t"
    + "G* = " + ngini.toString         + "\n"
  )

  /**
   * Generates a new numeric column from this column by applying the specified function to each of its values.
   * Behaves analogous to Scala's map()
   * @param func The function to apply to each value
   * @return The derived numeric column
   */
  def derive(func : Double => Double) = new NumCol(values.map(func).map(Helper.round))

 /**
   * Generates a new non-numeric column from this column by applying the specified function to each of its values.
   * Behaves analogous to Scala's map()
   * @param func The function to apply to each value
   * @return The derived non-numeric column
   */
  def deriveStr(func : Double => String) = new StrCol(values.map(func))

}