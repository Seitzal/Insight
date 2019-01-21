package eu.seitzal.insight

import scala.util.Try
import eu.seitzal.funcsv.FunCSV

/**
 * A table of data.
 * Each data frame consists of a number of named variables (columns) and their
 * corresponding data series.
 * @param columns A map consisting of the data frame's variable names as keys
                  and their corresponding data columns as values.
 */
class DataFrame (columns : Vector[(String, Series)]) {

  /*--- COLUMN-BASED FUNCTIONS ---*/

  /**
   * All data included in the data frame as a map of variable names and series
   */
  def getCols : Map[String, Series] = columns.toMap

  /**
   * Used internally for data formatting.
   */
  lazy val colTuples : List[(String, Series)] = {
    val raw = columns.toList
    val cnames = raw.unzip._1
    // If rows are numbered, the row number should be in the first column
    val orderedcols = if(cnames.contains("#")) {
      val i = cnames.indexOf("#")
      val slices = raw splitAt i
      (slices._2.head :: slices._1) ::: slices._2.tail
    }else raw
    // NumSeries containing only ints should be displayed without decimal places
    val orderedcolswithproperints = for(coltuple <- orderedcols) yield {
      val rawcol = coltuple._2
      rawcol match {
        case NumSeries(values) => {
          if((values.map(Helper.isInt).foldLeft(true)(_ && _)))
            (coltuple._1, new StrSeries(
              values.map((x: Option[Double]) => Helper.toIntString(x))))
          else
            coltuple
        }
        case sc : StrSeries    => coltuple
        case _              => throw new InvalidSeriesException(coltuple._1)
      }
    }
    orderedcolswithproperints
  }

  /**
   * Extract a single data column from the dataset.
   * @param cname The variable name of the requested data column.
   * @return The extracted data column as a series.
   */
  def getCol(cname : String) : Series = {
    val index = columns indexWhere(_._1 == cname)
    if (index == -1) throw new ColNotFoundException(cname)
    else columns(index) match {
      case (cn : String, col : StrSeries) => col
      case (cn : String, col : NumSeries) => col
      case _  => throw new InvalidSeriesException(cname)
    }
  }

  /**
   * Extract a single data column from the dataset.
   * @param cname The variable name of the requested data column.
   * @return The extracted data column as a series.
   */
  def apply(cname : String) = getCol(cname)

  /**
   * Extract a single data row / observation from the dataset.
   * @param index The index of the requested row
   * @return The extracted row as a map from column name to value.
   *         Missing values are returned as "n.a.".
   */
  def getRow(index : Int) = columns.map {
    case (cname, col) => col(index) match {
      case Some(x) => (cname, x)
      case None    => (cname, "n.a.")
      case any     => (cname, any)
    }
  }

  /**
   * Extract a single data row / observation from the dataset.
   * @param index The index of the requested row
   * @return The extracted row as a map from column name to value.
   *         Missing values are returned as "n.a.".
   */
  def apply(index : Int) = getRow(index)

  /**
   * Extract a single numeric data column from the dataset.
   * @param cname The variable name of the requested data column.
   * @return The extracted data column as a numeric series.
   */
  def num(cname : String) : NumSeries = getCol(cname) match {
    case nc : NumSeries => nc
    case sc : StrSeries => throw new NotNumericException(cname)
    case _           => throw new InvalidSeriesException(cname)
  }

  /**
   * Extract a single non-numeric data column from the dataset.
   * Numeric columns will be converted before being returned.
   * @param cname The variable name of the requested data column.
   * @return The extracted data column as a string.
   */
  def str(cname : String) : StrSeries = getCol(cname) match {
    case sc : StrSeries => sc
    case NumSeries(v)   => new StrSeries(for(x <- v) yield x.toString)
    case _           => throw new InvalidSeriesException(cname)
  }

  /**
   * Adds a new data column to the data frame, and returns the resulting frame.
   * @param cname The variable name for the new data column
   * @param col The new data column as a series
   * @return The new dataset, which includes the additional column
   */
  def +(cname : String, col : Series) = {
    val newcol = (cname, col)
    new DataFrame(columns :+ newcol)
  }

  /**
   * Removes a data column from the dataset and returns the resulting frame.
   * @param cname The variable name of the unwanted data column
   * @return The new dataset, which omits the specified column
   */
  def -(cname : String) =
    new DataFrame(columns.filter(t => t._1 != cname))

  /**
   * Removes multiple data columns from the dataset and returns the resulting
   * frame.
   * @param cname The variable names of the unwanted data columns
   * @return The new dataset, which omits the specified columns
   */
  def --(cnames : String*) =
    new DataFrame(columns.filter(t => !cnames.contains(t._1)))

  /**
   * Returns a new data frame including only the specified data columns.
   * @param cname The variable names of the desired data columns
   * @return The new dataset, which includes only the specified columns
   */
  def $(cnames : String*) : DataFrame =
    new DataFrame(columns.filter(t => cnames.contains(t._1)))

  /**
   * Returns the data frame with a column of 0-based row numbers added.
   */
  def withRowNumbers = {
    val rownumbers = new NumSeries((
      for(i <- 0 until columns.head._2.length)
      yield Option(i.toDouble)
    ).toVector.par)
    new DataFrame((("#", rownumbers) +: columns))
  }

  /**
   * Creates a subset of the data frame, including only those rows in which a
   * given numeric variable fulfills a given predicate.
   * @param cname The name of the variable by which to filter
   * @param condition The predicate for filtering
   * @return The new data frame, which includes only rows matching the filter
   */
  def filter(cname : String, condition : Double => Boolean) : DataFrame =
    getCol(cname) match {
      case NumSeries(col) => {
        val rownums = for(
          i <- 0 until col.length
          if col(i).isDefined && condition(col(i).getOrElse(0))
        ) yield i
        val newmap = for((n, c) <- columns) yield {
          val newc = c match {
            case NumSeries(v) => {
              val newvec = (for(rn <- rownums) yield v(rn)).toVector.par
              new NumSeries(newvec)
            }
            case StrSeries(v) => {
              val newvec = (for(rn <- rownums) yield v(rn)).toVector.par
              new StrSeries(newvec)
            }
            case _         => throw new InvalidSeriesException(cname)
          }
          (n, newc)
        }
        new DataFrame(newmap)
      }
      case StrSeries(col) => throw new NotNumericException(cname)
      case _           => throw new InvalidSeriesException(cname)
    }

  /**
   * Creates a subset of the data frame, including only those rows in which a
   * given numeric variable has a given value.
   * @param cname The name of the variable by which to filter
   * @param condition The value for filtering
   * @return The new data frame, which includes only rows matching the filter
   */
  def filter(cname : String, value : Double) : DataFrame =
    filter(cname, (x : Double) => x == value)


    /**
   * Creates a subset of the data frame, including only those rows in which a
   * given non-numeric variable fulfills a given predicate.
   * @param cname The name of the variable by which to filter
   * @param condition The predicate for filtering
   * @return The new data frame, which includes only rows matching the filter
   */
  def filterS(cname : String, condition : String => Boolean) : DataFrame = {
    val col = getCol(cname).asStrList.toVector
    val rownums = for(i <- 0 until col.length if condition(col(i))) yield i
    val newmap = for((n, c) <- columns) yield {
      val newc = c match {
        case NumSeries(v) => {
          val newvec = (for(rn <- rownums) yield v(rn)).toVector.par
          new NumSeries(newvec)
        }
        case StrSeries(v) => {
          val newvec = (for(rn <- rownums) yield v(rn)).toVector.par
          new StrSeries(newvec)
        }
        case _         => throw new InvalidSeriesException(cname)
      }
      (n, newc)
    }
    new DataFrame(newmap)
  }

  /**
   * Creates a subset of the data frame, including only those rows in which a
   * given non-numeric variable has a given value.
   * @param cname The name of the variable by which to filter
   * @param condition The predicate for filtering
   * @return The new data frame, which includes only rows matching the filter
   */
  def filterS(cname : String, value : String) : DataFrame =
    filterS(cname, (s : String) => s == value)

  /**
   * Divides the data into a number of value classes, returning a new data frame
   * which contains the number of elements in each class.
   * @param cname The name of the column by which to aggregate the data
   * @param firstCeiling The ceiling value of the lowest class
   * @param width The width of all classes after the lowest
   * @param k The total number of classes
   */
  def aggregateByValue(cname : String, firstCeiling : Double, width : Double,
      k : Int) =
    Aggregation.aggregateByValue(this, cname, firstCeiling, width, k)

  /**
   * Returns a list of threshold values between n equally sized brackets.
   * @param cname The name of the column representing the data for bracketing
   * @param n The number of brackets (The returned list will be n - 1 long)
   */
  def quantiles(cname : String, n : Int) : List[Double] =
    num(cname).quantiles(n)

  /**
   * Returns which out of n equally sized brackets a value x would be in.
   * @param cname The name of the column representing the data for bracketing
   * @param n The number of brackets
   * @param x The value for which the percentile should be found
   */
  def quantile(cname : String, n : Int, x : Double) : Int =
    num(cname).quantile(n, x)

  /*--- ROW-BASED FUNCTIONS ---*/

  /**
   * Transforms the data frame into a list of data rows.
   * Each row is represented as a sequence of String objects.
   * Numeric values are transformed to their decimal representations.
   * The first row contains the variable names.
   */
  lazy val getRows : List[List[String]] = {
    val unzipped = colTuples.unzip
    val cnames = unzipped._1
    val cols = for(col <- unzipped._2) yield col.asStrList
    val otherrows = for(i <- 0 until cols.head.length) yield(
      for(col <- cols) yield col(i)
    )
    cnames :: otherrows.toList
  }

  private object tabhelper {
    def longest(col : List[String], current : Int = 0) : Int = {
      if(col.isEmpty)
        current
      else if(col.head.length > current)
        longest(col.tail, col.head.length)
      else
        longest(col.tail, current)
    }

    lazy val longestPerCol = {
      val cols = for(col <- colTuples) yield col._1 :: col._2.asStrList
      for(col <- cols) yield longest(col)
    }.toList

    def whitespace(word : String, coln : Int) = (
      for(i <- 0 to (longestPerCol(coln) - word.length) + 1)
      yield " "
    ).mkString

    def dashes(coln : Int) = (
      for(i <- 0 to longestPerCol(coln) + 2)
      yield "-"
    ).mkString
  }

  /**
   * A simple table representation of the data frame,
   * with whitespace between values.
   */
  def tab : String = {
    def catrow(row : List[String]) = (for(i <- 0 until row.length) yield {
      val word = row(i)
      word + tabhelper.whitespace(word, i)
    }).mkString
    val rows = getRows.map(catrow)
    "\n" + rows.head + "\n\n" +
    (for(row <- rows.tail) yield row + "\n").mkString
  }

  /**
   * A table representation of the data frame, with lines between values.
   */
  def tabulate : String = {
    def catrow(row : List[String]) = (
    "| "
    + (for(i <- 0 until row.length) yield {
        val word = row(i)
        " " + word + tabhelper.whitespace(word, i) + "|"
      }).mkString
    + "\n"
    + "+-"
    + (for(i <- 0 until row.length) yield {
      tabhelper.dashes(i) + "+"
      }).mkString
    )
    val rows = getRows.map(catrow)
    "\n" + rows.head + "\n" +
    (for(row <- rows.tail) yield row + "\n").mkString
  }

  /**
   * Sorts the data frame by one of its columns, using a chosen sort mode.
   * @param cname The data column by which to sort the dataset.
   * @param mode The sorting mode. (Options in [[eu.seitzal.insight.SortMode]])
   * @return The sorted dataset.
   */
  def sort(cname : String, mode : SortMode = SortMode.ASCENDING) : DataFrame = {
    val heads = getRows.head
    val coln = (
      for(i <- 0 until heads.length if heads(i) == cname)
      yield i
    ).head
    val rows = getRows.tail
    val definedRows = rows.filter(row => row(coln) != "--")
    val undefinedRows = rows.filter(row => row(coln) == "--")
    val newRows = getCol(cname) match {
      case NumSeries(v) => definedRows.sortBy(row => row(coln).toDouble)
      case StrSeries(v) => definedRows.sortBy(row => row(coln))
      case _         => throw new ColNotFoundException(cname)
    }
    mode match {
      case SortMode.ASCENDING =>
        DataFrame.fromRows(heads :: (newRows ++ undefinedRows))
      case SortMode.DESCENDING =>
        DataFrame.fromRows(heads :: (newRows.reverse ++ undefinedRows))
    }
  }

  /*-- Multicolumn function shortcuts --*/

  /**
   *  Calculates the covariance of two variables in the data frame
   *  @param cname1 The name of the data column representing the first variable
   *  @param cname2 The name of the data column representing the second variable
   */
  def cov(cname1 : String, cname2 : String) =
    num(cname1) cov num(cname2)

  /**
   *  Calculates Pearson's correlation coefficient for two variables in the
   *  data frame
   *  @param cname1 The name of the data column representing the first variable
   *  @param cname2 The name of the data column representing the second variable
   */
  def pearson(cname1 : String, cname2 : String) =
    num(cname1) pearson num(cname2)

  /**
   *  Calculates the partial correlation between two variables,
   *  while keeping a third variable constant.
   *  @param cname1 The name of the data column representing the first variable
   *  @param cname2 The name of the data column representing the second variable
   *  @param cnameControl The name of the data column representing the
   *                      disturbing variable
   */
  def pearsonPartial(cname1 : String, cname2 : String,
      cnameControl : String) : Double =
    num(cname1).pearsonPartial(num(cname2), num(cnameControl))

  /**
   *  Builds a correlation matrix containing the correlation coefficients
   *  between all numeric variables in the dataset,
   *  Using Pearson's r as a correlation measure.
   */
  lazy val pearsonMatrix : CorrelationMatrix = {
    val numcols = colTuples.filter(
      (item : (String, Series)) => item._2.isInstanceOf[NumSeries]
    )
    def vec = (
      for (nc1 <- numcols) yield (
        for (nc2 <- numcols)
        yield Helper.roundTo(
          nc1._2.asInstanceOf[NumSeries] pearson
          nc2._2.asInstanceOf[NumSeries], 5)
      ).toVector
    ).toVector
    new CorrelationMatrix(colTuples.unzip._1.toVector, vec)
  }

}

/**
 * The companion object for the dataset type.
 * Contains methods for creating dataset objects from various sources.
 */
object DataFrame {

  /**
   * Attempts to read a dataset from a CSV file at the specified location.
   * The first line of the CSV is assumed to contain the variable names.
   * @param path The path to the CSV file.
   * @return A dataset object containing the data from the file.
   */
  def fromCSV(path : String) : DataFrame = {
    fromRows(FunCSV.decodeFile(path))

  }

  /**
   * Attempts to create a dataset from a list of rows,
   * each row being itself a list of string-formatted values.
   */
  def fromRows(raw : List[List[String]]) : DataFrame = {
    // DataFrames may not include multiple columns of the same name
    if(!Helper.nodoubles(raw.head)) throw new NameNotUniqueException()

    val rawcols = for(i <- 0 until raw.tail.head.length) yield(
      for(row <- raw.tail) yield row(i)
    )
    val cols = for(rawcol <- rawcols) yield parseCol(rawcol)
    new DataFrame((raw.head zip cols).toVector)
  }

  /**
   * Infers the datatype of a column from its values
   */
  private def parseCol(raw : List[String]) : Series = {
    def tryconvert (item : String) : Option[Option[Double]] =
      if (item == "" || item == "-" || item.equalsIgnoreCase("na")) Option(None)
      else {
        val tryconvert2 = Try{item.toDouble}.toOption
        tryconvert2 match {
          case Some(x) => Option(tryconvert2)
          case None    => None
        }
      }
    val triedlist = raw.map(tryconvert)
    if(triedlist.forall(_.isDefined))
      new NumSeries(triedlist.map(_.get).toVector.par)
    else
      new StrSeries(raw.toVector.par)
  }
}
