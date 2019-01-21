package eu.seitzal.insight

import scala.io.Source
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ArrayBuffer

/**
 * Represents a table of data that is not read into memory completely.
 * Each lazy frame consists of a number of named variables (columns) and their
 * corresponding data series.
 * Data is only read from the source (generally a file) whe it is explicitly
 * requested or required by some other function.
 * This allows for constant-time initialisation and a smaller memory footprint,
 * but results in significantly slower lookups.
 */
class LazyFrame(source : LazyFrameSource) {

  /**
   * Parses a row of comma-separated values
   */
  private def csvParseRow(plain : String) : Vector[String] = {
    val len = plain.length
    def iter(i : Int, escaped : Boolean, buffer : String,
        acc : Vector[String]) : Vector[String] =
      if (i == len) acc
      else if (plain.charAt(i) == '\"')
        if (plain.substring(i + 1, i + 3) == "\"\"")
          iter(i + 1, escaped, buffer + "\"", acc)
        else
          iter(i + 1, !escaped, buffer, acc)
      else if (plain.charAt(i) == ',')
        if (escaped)
          iter(i + 1, escaped, buffer + ",", acc)
        else
          iter(i + 1, escaped, "", acc :+ buffer)
      else
        iter(i + 1, escaped, buffer + plain.charAt(i), acc)
    iter(0, false, "", Vector())
  }

  /**
   * Parses a row of comma-separated values, looking for a specific cell
   */
  private def csvParseRowForCell(plain : String, col : Int) : Option[String] = {
    val len = plain.length
    def iter(i : Int, in_col : Int, escaped : Boolean,
        buffer : String) : Option[String] =
      if (i == len)
        if (in_col == col)
          Some(buffer)
        else
          None
      else if (plain.charAt(i) == '\"')
        if (plain.substring(i + 1, i + 3) == "\"\"")
          if (in_col == col)
            iter(i + 1, in_col, escaped, buffer + "\"")
          else
            iter(i + 1, in_col, escaped, buffer)
        else
          iter(i + 1, in_col, !escaped, buffer)
      else if (plain.charAt(i) == ',')
        if (escaped)
          if (in_col == col)
            iter(i + 1, in_col, escaped, buffer + ",")
          else
            iter(i + 1, in_col, escaped, buffer)
        else
          if (in_col == col)
            Some(buffer)
          else
            iter(i + 1, in_col + 1, escaped, buffer)
      else if (in_col == col)
        iter(i + 1, in_col, escaped, buffer + plain.charAt(i))
      else
        iter(i + 1, in_col, escaped, buffer)
    iter(0, 0, false, "")
  }

  /**
   * Loads a row of data from the source
   * @param index The index of the row. Row 0 contains the column names
   */
  private def loadRow(index : Int) : Vector[String] = source match {
    case LazyFrameSource.FileCSV(path) => {
      val src = Source.fromFile(path).getLines
      src.drop(index - 1)
      if (src.hasNext)
        csvParseRow(src.next)
      else
        throw new RowNotFoundException(index)
    }
    case any => throw new Error("LazyFrame has unknown source type")
  }

  /**
   * Loads a column of data from the source
   * @tparam T The type of values to load
   * @tparam U The type of data structure to return
   * @param col The index of the column
   * @param processor The function to convert each string value to T
   * @param wrapper The function to convert the internal buffer to type U
   * @param limit How many rows of data to parse. Set to -1 to parse all rows
   */
  private def loadCol[T, U](
      col : Int,
      processor : String => T,
      wrapper : ArrayBuffer[T] => U,
      limit : Int = -1) : U = source match {
    case LazyFrameSource.FileCSV(path) => {
      val src = Source.fromFile(path).getLines
      src.drop(1)
      val buffer = new ArrayBuffer[T]()
      var i = 0
      while (src.hasNext && i != limit) {
        csvParseRowForCell(src.next, col) match {
          case Some(x) => buffer += processor(x)
          case None    => throw new Error("Error: Malformed dataset")
        }
        i += 1
      }
      wrapper(buffer)
    }
    case any => throw new Error("LazyFrame has unknown source type")
  }

  /**
   * Tries to convert a string to a numeric option,
   * encoding missing values as empty options.
   * Throws an exception if it encounters a non-numeric value.
   */
  private def processNum(cname : String)(item : String) : Option[Double] =
    if (item.equals("") || item.equals("-") ||
        item.equalsIgnoreCase("na"))
      None
    else Try(item.toDouble) match {
      case Success(x) => Some(x)
      case Failure(e) => throw new NotNumericException(cname)
    }

  /**
   * Wraps a buffer of numeric options into a numeric series.
   */
  private def wrapNum(buffer : ArrayBuffer[Option[Double]]) =
    NumSeries(buffer.toVector.par)

  /**
   * Extract a single numeric data column from the dataset.
   * @param cname The variable name of the requested data column.
   * @return The extracted data column as a numeric series.
   */
  def num(cname : String) : NumSeries = cnames.indexOf(cname) match {
    case -1    => throw new ColNotFoundException(cname)
    case index => loadCol(index, processNum(cname), wrapNum)
  }

  /**
   * Wraps a buffer of strings into a non-numeric series.
   */
  private def wrapStr(buffer : ArrayBuffer[String]) =
    StrSeries(buffer.toVector.par)

  /**
   * Extract a single non-numeric data column from the dataset.
   * Numeric columns will be converted before being returned.
   * @param cname The variable name of the requested data column.
   * @return The extracted data column as a string.
   */
  def str(cname : String) : StrSeries = cnames.indexOf(cname) match {
    case -1    => throw new ColNotFoundException(cname)
    case index => loadCol(index, str => str, wrapStr)
  }

  /**
   * Extract a single data column from the dataset.
   * @param cname The variable name of the requested data column.
   * @return The extracted data column as a series.
   */
  def apply(cname : String) : Series = {
    Try(num(cname)) match {
      case Success(nums) => nums
      case Failure(e : NotNumericException) => str(cname)
      case Failure(e) => throw e
    }
  }

  /**
   * A vector containing all variable names in the data frame.
   */
  lazy val cnames = loadRow(0)

}

object LazyFrame {
  def fromCSV(path : String) =
    new LazyFrame(LazyFrameSource.FileCSV(path))
}

trait LazyFrameSource
object LazyFrameSource {
  case class FileCSV(val path : String) extends LazyFrameSource
}