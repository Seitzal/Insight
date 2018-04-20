package seitzal.scalastat

import scala.annotation.tailrec

/**
 * Represents a data column containing only numeric values (quantifiable on an interval or ratio scale).
 * Missing values are not currently supported.
 */
case class NumCol (values : Vector[Option[Double]]) extends Col {

  lazy val length = values.length

  lazy val existingValues =
    values withFilter {
      case Some(value) => true
      case None        => false
    } map (_.get)

  lazy val existingLength = existingValues.length

  lazy val asStrList = 
    for(e <- values.toList) yield e match {
      case Some(value) => value.toString
      case None        => "-"
    }
      
  override def toString = {
    val withtrailingcomma = (for(e <- asStrList) yield e + ", ").mkString
    withtrailingcomma.substring(0, withtrailingcomma.length - 2)
  }

  /**
   * The sum of all values of the column.
   * @return The sum of the column.
   */
  lazy val sum = Helper.round(existingValues.foldLeft(0.0)(_ + _))

  /**
   * The arithmetic mean of all values of the column.
   */
  lazy val avg = Helper.round(sum / existingLength)

  private lazy val sortedvs = existingValues.sorted

  /**
   * The median value of the column.
   */
  lazy val median = (existingLength % 2) match {
    case 1 => sortedvs(existingLength / 2)
    case 0 => (sortedvs(existingLength / 2 - 1) + sortedvs(existingLength / 2)) / 2
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
   * 
   * This function will return 0s in place of empty values to facilitate followup steps, such as cumulation.
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
    new NumCol(loop(relfreq.values.toList.map(_.getOrElse(0.0)), Vector[Double](), 0).map(Helper.round).map(Option(_)))
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
    new NumCol(loop(values.toList.map(_.getOrElse(0.0)), Vector[Double](), 0).map(Helper.round).map(Option(_)))
  }

  /**
   * Calculates the Gini concentration index for an unaggregated data column.
   * The gini index measures the relative concentration of a frequency distribution.
   * It is defined as the area between the distribution's Lorenz curve and the main diagonal f(x) = x.
   */
  lazy val gini = {
    val numerator = 2 * (
      (for (i <- 0 until existingLength) yield (i + 1) * existingValues(i))
      .foldLeft(0.0)(_ + _)
    )
    val subtrahend = (existingLength.toDouble + 1) / existingLength.toDouble
    Helper.round(numerator / (sum * existingLength) - subtrahend)
  }

  /**
   * Calculates the normalised Gini concentration index for an unaggregated data column.
   * The result is guaranted to be in [0;1] even for very short / small-n columns.
   */
  lazy val ngini = Helper.round(gini * (existingLength.toDouble / (existingLength.toDouble - 1)))

  /**
   * Calculates the Gini concentration index for an aggregated data column, assuming equal class breadth and even distribution within classes.
   * The gini index measures the relative concentration of a frequency distribution.
   * It is defined as the area between the distribution's Lorenz curve and the main diagonal f(x) = x.
   */
  lazy val cgini = {
    values.map {
      case None    => throw new MissingValueException("aggregate Gini index")
      case _       => 0
    }
    val hstar = 1 / length.toDouble
    Helper.round(((for (i <- 0 until length) yield {
      if(i == 0)
        hstar * crelfreq.values(i).get
      else
        hstar * (crelfreq.values(i - 1).get + crelfreq.values(i).get)
    }).foldLeft(0.0)(_ + _)) - 1)
  }

  /**
   * Calculates the column's Robin Hood / Hoover Index of relative concentration.
   * The RHI is defined as the amount that would need to be redistributed in order to achieve even distribution.
   */
  lazy val rhi = {
    val upperlist = existingValues.filter(_ >= avg)
    val numerator = upperlist.map((x : Double) => x - avg).foldLeft(0.0)(_ + _)
    Helper.round(numerator / sum)
  }

  /**
   * Calculates the column's concentration rate, defined as the sum of the column's m largest elements divided by the sum of all elements.
   * @param m The number of elements to include in the numerator sum
   */
  def crate (m : Int) = {
    val numerator = existingValues.sorted.takeRight(m).foldLeft(0.0)(_ + _)
    Helper.round(numerator / sum.toDouble)
  }


  /** Calculates the Herfindahl index of absolute concentration.
   *  The Herfindahl index is defined as the sum of the squared relative frequencies of all elements.
   *  Values for this index range from 1/n (even distribution) to 1 (maximal concentration)
   */
  lazy val herfindahl = Helper.round(relfreq.derive((x : Double) => math.pow(x, 2)).sum)

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

  /** Calculates the covariance for the variables represented by this column and another given column.
   *  This ignores any rows with missing values in either column.
   *  @param otherCol The data column representing the other variable
   */
  def cov(otherCol : NumCol) : Double = {
    if (length == otherCol.length) {
      val rawx = values;
      val rawy = otherCol.values;
      val intactrows = for(
        i <- 0 until length 
        if (rawx(i).isDefined && rawy(i).isDefined)
      ) yield (rawx(i).get, rawy(i).get)
      val intactlength = intactrows.length
      val x = intactrows.unzip._1
      val y = intactrows.unzip._2
      val products = 
        for (i <- 0 until length)
        yield x(i) * y(i)
      val sumofproducts = products.foldLeft(0.0)(_ + _)
      Helper.round((sumofproducts / length) - (avg * otherCol.avg))             
    } else throw new ColLengthException("covariance");
  }

  /** Calculates pearson's correlation coefficient for the variables represented by this column and another given column.
   *  @param otherCol The data column representing the other variable
   */
  def pearson(otherCol : NumCol) : Double = 
    Helper.round(cov(otherCol) / (standardDev * otherCol.standardDev))

  /** Calculates the partial correlation between this variable and another variable while keeping a third variable constant.
   *  @param otherCol The data column representing the other variable
   *  @param controlledCol The data column representing the disturbing variable
   */
  def pearsonPartial(otherCol : NumCol, controlledCol : NumCol) : Double = {
    val xy = this pearson otherCol
    val xz = this pearson controlledCol
    val yz = otherCol pearson controlledCol
    val numerator = xy - xz * yz
    val root1 = math.sqrt(1 - math.pow(xz, 2))
    val root2 = math.sqrt(1 - math.pow(yz, 2))
    Helper.round(numerator / (root1 * root2))
  }

  /** Performs a simple linear regression using the ordinary least squares method.
   *  The column from which this function is invoked is assumed to represent the dependent variable.
   *  @param independent The column containing the independent veriable.
   *  @return A tuple containing the regression's y intersect and slope, in that order. 
   */
  def ols(independent : NumCol) : (Double, Double) = {
    val slope = cov(independent) / independent.variance
    val yintersect = avg - slope * independent.avg
    (Helper.round(yintersect), Helper.round(slope))
  }

  /**
   * Generates a new numeric column from this column by applying the specified function to each of its values.
   * Behaves analogous to Scala's map()
   * @param func The function to apply to each value
   * @return The derived numeric column
   */
  def derive(func : Double => Double) =
    new NumCol (
      for(e <- values) yield e match {
        case Some(x) => Option(Helper.round(func(x)))
        case None    => None
      }
    )


 /**
   * Generates a new non-numeric column from this column by applying the specified function to each of its values.
   * Behaves analogous to Scala's map()
   * @param func The function to apply to each value
   * @return The derived non-numeric column
   */
  def deriveStr(func : Double => String) =
    new StrCol (
      for(e <- values) yield e match {
        case Some(x) => func(x)
        case None    => "-" 
      }
    )
}

object NumCol {
  def make(values : Vector[Double]) = new NumCol(values.map(Option(_)))
}
