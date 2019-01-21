package eu.seitzal.insight

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector

/**
 * A series containing only numeric values.
 */
case class NumSeries (values : ParVector[Option[Double]]) extends Series {

  /**
   * Returns the value at the given index.
   * None if the value doesn't exist.
   */
  def apply(index : Int) = values(index)

  /**
   * The number of values in the series, including blanks.
   */
  val length = values.length

  /**
   * The number of values in the series, excluding blanks.
   */
  def existingLength = existingValues.length

  /**
   * A vector of doubles containing all defined values in this series,
   * with blanks filtered out.
   */
  def existingValues =
    values.filter(_.isDefined).map(_.get)

  /**
   * Compares this series with another numeric series,
   * filtering out any values that aren't defined on either side.
   * This maintains pairs, shifting their indices together.
   * @param that The series to compare with this series.
   * @return A tuple of vectors, wherein the first vector corresponds to this 
   *         series, and the second to the other one.
   */
  def commonExistingValues(that : NumSeries) :
      (Vector[Double], Vector[Double]) = {
    val shorterLength =
      if(this.length <= that.length) this.length
      else that.length
    def iter(x1 : Vector[Double], x2 : Vector[Double], i : Int) :
        (Vector[Double], Vector[Double]) = 
      if(i == shorterLength)
        (x1, x2)
      else if(this.values(i).isDefined && that.values(i).isDefined)
        iter(x1 :+ this.values(i).get, x2 :+ that.values(i).get, i + 1)
      else
        iter(x1, x2, i + 1)
    iter(Vector[Double](), Vector[Double](), 0)
  }

  /**
   * Compares this series with another numeric series, 
   * filtering out any values that aren't defined on either side, 
   * and re-wrapping both vectors into NumSeries objects.
   * This maintains pairs, but will shift their index.
   * @param that The series to compare with this series.
   * @return A tuple of NumSeries objects, wherein the first object corresponds 
   *         to this series, and the second to the other one.
   */
  def commonExisting(that : NumSeries) : (NumSeries, NumSeries) = {
    val commonExistingValues = this.commonExistingValues(that) 
    (new NumSeries(commonExistingValues._1.map(Option(_)).par), 
      new NumSeries(commonExistingValues._2.map(Option(_)).par)) 
  }

  /** 
   * Compares this series with another numeric series, 
   * returning the number of value pairs that are defined in both.
   */
  def commonExistingLength(that : NumSeries) : Int = {
    val shorterLength =
      if(this.length <= that.length) this.length
      else that.length
    def iter(n : Int, i : Int) : Int =
      if(i == shorterLength)
        n
      else if(this.values(i).isDefined && that.values(i).isDefined)
        iter(n + 1, i + 1)
      else
        iter(n, i + 1)
    iter(0, 0)
 }

  def asStrList = (values map {
    case Some(value) => value.toString
    case None        => "NA"
  }).seq.toList
      
  override def toString = {
    val withtrailingcomma = (for(e <- asStrList) yield e + ", ").mkString
    "NumSeries (" +
    withtrailingcomma.substring(0, withtrailingcomma.length - 2) +
    ")"
  }

  /**
   * The sum of all values of the series.
   * @return The sum of the series.
   */
  def sum = existingValues.fold(0.0)(_ + _)

  /**
   * The arithmetic mean of all values of the series.
   */
  def avg = sum / existingLength

  private def sortedvalues = existingValues.seq.sorted

  /**
   * The median value of the series.
   */
  def median = (existingLength % 2) match {
    case 1 => 
      sortedvalues(existingLength / 2)
    case 0 => 
      (sortedvalues(existingLength / 2 - 1) +
      sortedvalues(existingLength / 2)) / 2
  }

  /**
   * The largest value in the series.
   */
  def max = {
    def iter(xs : List[Double], current : Double) : Double =
      if (xs.isEmpty) current
      else if (xs.head > current) iter(xs.tail, xs.head)
      else iter(xs.tail, current)
    iter(existingValues.toList, existingValues.head)
  }

  /**
   * The smallest value in the series.
   */
  def min = {
    def iter(xs : List[Double], current : Double) : Double =
      if (xs.isEmpty) current
      else if (xs.head < current) iter(xs.tail, xs.head)
      else iter(xs.tail, current)
    iter(existingValues.toList, existingValues.head)
  }

  /**
   * A series containing the deviations of each element in this series from the 
   * arithmetic mean
   */
  def devs = derive(_ - avg)

  /**
   * A series containing the squared deviations of each element in this series
   * from the arithmetic mean
   */
  def devsSquared = derive((x : Double) => (x - avg) * (x - avg))

  /**
   * The variance of the series.
   */
  def variance = devsSquared.avg

  /**
   * The standard deviation of the series
   */
  def standardDev = Helper.round(math.sqrt(variance))
  
  /**
   * Calculates the n-th central moment of a normal distribution
   * @param n the degree of the central moment
   */
  def centralMoment(n : Int) = {
    val items = for(x <- existingValues)
                yield math.pow((x - avg), n)
    Helper.round(items.foldLeft(0.0)(_ + _) / existingLength)
  }

  /**
   * Calculates the skewness of a normal distribution
   */
  def skewness = centralMoment(3) / (variance * standardDev)

  /**
   * Calculates the kurtosis of a normal distribution
   */
  def kurtosis = centralMoment(4) / (variance * variance) - 3

  /**
   * A derived series containing the relative frequencies corresponding 
   * to the absolute frequencies in this series.
   * This function will return 0s in place of empty values
   */
  def relfreq = derive(_ / sum)

  /**
   * A series containing the cumulative relative frequencies
   * corresponding to the absolute frequencies in the series.
   * Cumulation is done from the head down the tail of the value list,
   * i.e. top-to-bottom when imagining a table.
   */
  def crelfreq = {
    @tailrec def loop(remainder : List[Double], acc : Vector[Double], 
        prev : Double) : Vector[Double] =
      if(remainder.isEmpty) acc
      else {
        val crf = remainder.head + prev
        loop(remainder.tail, acc :+ crf, crf)
      }
    new NumSeries(loop(relfreq.values.toList.map(_.getOrElse(0.0)), 
      Vector[Double](), 0).map(Option(_)).par)
  }

  /**
   * A series containing the cumulative absolute frequencies
   * corresponding to the absolute frequencies in the series.
   * Cumulation is done from the head down the tail of the value list,
   * i.e. top-to-bottom when imagining a table.
   */
  def cabsfreq = {
    @tailrec def loop(remainder : List[Double], acc : Vector[Double], 
        prev : Double) : Vector[Double] =
      if(remainder.isEmpty) acc
      else {
        val crf = remainder.head + prev
        loop(remainder.tail, acc :+ crf, crf)
      }
    new NumSeries(loop(values.toList.map(_.getOrElse(0.0)),
      Vector[Double](), 0).map(Option(_)).par)
  }

  /**
   * Calculates the Gini concentration index for an unaggregated data series.
   */
  def gini = {
    val numerator = 2 * (
      (for (i <- 0 until existingLength) yield (i + 1) * existingValues(i))
      .fold(0.0)(_ + _)
    )
    val subtrahend = (existingLength.toDouble + 1) / existingLength.toDouble
    numerator / (sum * existingLength) - subtrahend
  }

  /**
   * Calculates the normalised Gini concentration index for an unaggregated
   * data series.
   * The result is guaranted to be in [0;1] even for very short / small-n series
   */
  def ngini = 
    gini * (existingLength.toDouble / (existingLength.toDouble - 1))

  /**
   * Calculates the Gini concentration index for an aggregated data series,
   * assuming equal class breadth and even distribution within classes.
   */
  def cgini = {
    values.map {
      case None    => throw new MissingValueException("aggregate Gini index")
      case _       => 0
    }
    val hstar = 1 / length.toDouble
    ((for (i <- 0 until length) yield {
      if(i == 0)
        hstar * crelfreq.values(i).get
      else
        hstar * (crelfreq.values(i - 1).get + crelfreq.values(i).get)
    }).fold(0.0)(_ + _)) - 1
  }

  /**
   * Calculates the series's Robin Hood Index of relative concentration.
   * The RHI is defined as the amount that would need to be redistributed 
   * in order to achieve even distribution.
   */
  def rhi = {
    val upperlist = existingValues.filter(_ >= avg)
    val numerator = upperlist.map((x : Double) => x - avg).fold(0.0)(_ + _)
    numerator / sum
  }

  /**
   * Returns a list of threshold values between n equally sized brackets of
   * values.
   * @param n The number of brackets (The returned list will be n - 1 long)
   */
  def quantiles (n : Int) : List[Double] = {
    if (n <= 0) throw new ZeroCountException("Number of n-tiles")
    val step : Double = existingLength.toDouble / n
    (for (i <- step until existingLength by step) 
    yield sortedvalues(i.toInt)).toList
  }

  /**
   * Returns which out of n equally sized brackets a value x would be in.
   * @param n The number of brackets
   * @param x The value for which the percentile should be found
   */
  def quantile(n : Int, x : Double) : Int = {
    def iter(n : Int, remainder : List[Double]) : Int =
      if (x < remainder.head) n
      else if (remainder.isEmpty) n
      else iter(n + 1, remainder.tail)
    iter(1, quantiles(n))
  }

  /**
   * Calculates the series's concentration rate, defined as the sum of the 
   * series's m largest elements divided by the sum of all elements.
   * @param m The number of elements to include in the numerator sum
   */
  def crate (m : Int) = {
    val numerator = existingValues.seq.sorted.takeRight(m).foldLeft(0.0)(_ + _)
    numerator / sum.toDouble
  }


  /** Calculates the Herfindahl index of absolute concentration.
   *  The Herfindahl index is defined as the sum of the squared relative 
   *  frequencies of all elements.
   *  Values for this index range from 1/n (even distribution) to 1 
   *  (maximal concentration)
   */
  def herfindahl = relfreq.derive((x : Double) => math.pow(x, 2)).sum

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

  /** Calculates the covariance for the variables represented by this series
   *  and another given series.
   *  This ignores any rows with missing values in either series.
   *  @param otherCol The data series representing the other variable
   */
  def cov(otherCol : NumSeries) : Double = {
    val commexc = commonExisting(otherCol)
    val xs = commexc._1.existingValues
    val ys = commexc._2.existingValues
    val l = xs.length
    val products =
      for (i <- 0 until l)
      yield xs(i) * ys(i)
    (products.foldLeft(0.0)(_ + _) / l) - commexc._1.avg * commexc._2.avg
  }

  /** Calculates pearson's correlation coefficient for the variables represented 
   *  by this series and another given series.
   *  @param otherCol The data series representing the other variable
   */
  def pearson(otherCol : NumSeries) : Double = {
    val commexc = commonExisting(otherCol)
    cov(otherCol) / (commexc._1.standardDev * commexc._2.standardDev)
  }

  /** Calculates the partial correlation between this variable and another 
   *  variable while keeping a third variable constant.
   *  @param otherCol The data series representing the other variable
   *  @param controlledCol The data series representing the disturbing variable
   */
  def pearsonPartial(otherCol : NumSeries, 
      controlledCol : NumSeries) : Double = {
    val xy = this pearson otherCol
    val xz = this pearson controlledCol
    val yz = otherCol pearson controlledCol
    val numerator = xy - xz * yz
    val root1 = math.sqrt(1 - math.pow(xz, 2))
    val root2 = math.sqrt(1 - math.pow(yz, 2))
    numerator / (root1 * root2)
  }

  /**
   * Generates a new numeric series from this series by applying the specified
   * function to each of its values.
   * Behaves analogous to Scala's map()
   * @param func The function to apply to each value
   * @return The derived numeric series
   */
  def derive(func : Double => Double) =
    new NumSeries (
      values.map {
        case Some(x) => Option(func(x))
        case None    => None
      }
    )

 /**
   * Generates a new non-numeric series from this series by applying the 
   * specified function to each of its values.
   * Behaves analogous to Scala's map()
   * @param func The function to apply to each value
   * @return The derived non-numeric series
   */
  def deriveStr(func : Double => String) =
    new StrSeries (
      values.map {
        case Some(x) => func(x)
        case None    => "NA" 
      }
    )
}

/**
 * The companion object of the NumSeries class. 
 * Contains a shorthand constructor to create a NumSeries from a parameter list:
 * <pre><code>val myNumSeries = NumSeries(2, 5, 3.24, -12)</code></pre>
 */
object NumSeries {
  def apply(values : Double*) =
    new NumSeries(values.map(Option(_)).toVector.par)
}
