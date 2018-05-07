package eu.seitzal.scalastat

import scala.annotation.tailrec

/**
 * Represents a data column containing only numeric values (quantifiable on an interval or ratio scale).
 * Missing values are not currently supported.
 */
case class NumCol (values : Vector[Option[Double]]) extends Col {

  /**
   * The number of values in the column, including blanks.
   */
  lazy val length = values.length

  /**
   * The number of values in the column, excluding blanks.
   */
  lazy val existingLength = existingValues.length

  /**
   * A vector of doubles containing all defined values in this column, with blanks filtered out.
   * When iterating over this, use existingLength (not length) as an exclusive upper bound.
   */
  lazy val existingValues =
    values withFilter {
      case Some(value) => true
      case None        => false
    } map (_.get)

  /**
   * Compares this column with another numeric column, filtering out any values that aren't defined on either side.
   * This maintains pairs, but will shift their index.
   * @param that The column to compare with this column.
   * @return A tuple of vectors, wherein the first vector corresponds to this column, and the second to the other one.
   */
  def commonExistingValues(that : NumCol) : (Vector[Double], Vector[Double]) = {
    val shorterLength =
      if(this.length <= that.length) this.length
      else that.length
    def iter(x1 : Vector[Double], x2 : Vector[Double], i : Int) : (Vector[Double], Vector[Double]) = 
      if(i == shorterLength)
        (x1, x2)
      else if(this.values(i).isDefined && that.values(i).isDefined)
        iter(x1 :+ this.values(i).get, x2 :+ that.values(i).get, i + 1)
      else
        iter(x1, x2, i + 1)
    iter(Vector[Double](), Vector[Double](), 0)
  }

  /**
   * Compares this column with another numeric column, filtering out any values that aren't defined on either side, 
   * and re-wrapping both vectors into NumCol objects.
   * This maintains pairs, but will shift their index.
   * @param that The column to compare with this column.
   * @return A tuple of NumCol objects, wherein the first object corresponds to this column, and the second to the other one.
   */
  def commonExistingCols(that : NumCol) : (NumCol, NumCol) = {
    val commonExistingValues = this.commonExistingValues(that) 
    (new NumCol(commonExistingValues._1.map(Option(_))), new NumCol(commonExistingValues._2.map(Option(_)))) 
  }

  /** 
   * Compares this column with another numeric column, returning the number of value pairs that are defined in both columns.
   */
  def commonExistingLength(that : NumCol) : Int = {
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
   * The unrounded sum of all values in the column.
   * This will most likely result in weird periodic outputs.
   * For most operations, sum should be used.
   * However, when dealing with very large numbers, the internal rounding function can cause severe errors.
   * In such cases, safeSum provides a working sum that can be used for calculations.
   */
  lazy val safeSum = existingValues.foldLeft(0.0)(_ + _)

  /**
   * The arithmetic mean of all values of the column.
   */
  lazy val avg = Helper.round(safeSum / existingLength)

  private lazy val sortedvalues = existingValues.sorted

  /**
   * The median value of the column.
   */
  lazy val median = (existingLength % 2) match {
    case 1 => sortedvalues(existingLength / 2)
    case 0 => (sortedvalues(existingLength / 2 - 1) + sortedvalues(existingLength / 2)) / 2
  }

  /**
   * The largest value in the column.
   */
  lazy val max = {
    def iter(xs : List[Double], current : Double) : Double =
      if (xs.isEmpty) current
      else if (xs.head > current) iter(xs.tail, xs.head)
      else iter(xs.tail, current)
    iter(existingValues.toList, existingValues.head)
  }

  /**
   * The smallest value in the column.
   */
  lazy val min = {
    def iter(xs : List[Double], current : Double) : Double =
      if (xs.isEmpty) current
      else if (xs.head < current) iter(xs.tail, xs.head)
      else iter(xs.tail, current)
    iter(existingValues.toList, existingValues.head)
  }

  /**
   * A derived column containing the deviations of each element in the column from the arithmetic mean
   */
  lazy val devs = derive(_ - avg)

  /**
   * A derived column containing the squared deviations of each element in the column from the arithmetic mean
   */
  lazy val devsSquared = derive((x : Double) => (x - avg) * (x - avg))

  /**
   * The variance of the column.
   * (The variance is the arithmetic mean of the squared deviations of each element from the arithmetic mean of the column)
   */
  lazy val variance = devsSquared.avg

  /**
   * The standard deviation of the column, defined as the square root of the column's variance
   */
  lazy val standardDev = Helper.round(math.sqrt(variance))
  
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
  lazy val skewness = Helper.round(centralMoment(3) / (variance * standardDev))

  /**
   * Calculates the kurtosis of a normal distribution
   */
  lazy val kurtosis = Helper.round(centralMoment(4) / (variance * variance) - 3)

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
    val commexc = commonExistingCols(otherCol)
    val xs = commexc._1.existingValues
    val ys = commexc._2.existingValues
    val l = xs.length
    val products =
      for (i <- 0 until l)
      yield xs(i) * ys(i)
    Helper.round((products.foldLeft(0.0)(_ + _) / l) - commexc._1.avg * commexc._2.avg)
  }

  /** Calculates pearson's correlation coefficient for the variables represented by this column and another given column.
   *  @param otherCol The data column representing the other variable
   */
  def pearson(otherCol : NumCol) : Double = {
    val commexc = commonExistingCols(otherCol)
    Helper.round(cov(otherCol) / (commexc._1.standardDev * commexc._2.standardDev))
  }

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

  /** Calculates the determination coefficient R² for simple linear regression from another variable to the variable in this column.
   *  This is equivalent to the squared Pearson r.
   */
  def rsq(independent : NumCol) : Double = {
    val r = pearson(independent)
    r * r
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

/**
 * The companion object of the NumCol class. 
 * Contains a shorthand constructor to create a NumCol from a parameter list:
 * <pre><code>val myNumericColumn = NumCol(2, 5, 3.24, -12)</code></pre>
 */
object NumCol {
  def apply(values : Double*) = new NumCol(values.map(Option(_)).toVector)
}
