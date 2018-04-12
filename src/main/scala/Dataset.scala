package seitzal.scalastat

import scala.util.Try

import seitzal.funcsv.FunCSV

/**
 * A table of data.
 * Each dataset consists of a number of named variables and their corresponding data columns.
 * In a valid dataset, all columns have the same length. Missing values are not currently supported.
 * Datasets can be read from CSV files using the factory object of this class. They can also be created directly by instantiating the class.
 * @param columns A map consisting of the dataset'S variable names as keys and their corresponding data columns as values.
 */
class Dataset (columns : Map[String, Col]) {

  /*--- COLUMN-BASED FUNCTIONS ---*/

  /**
   * @return All data included in the dataset as a map of variable names and data columns.
   */
  def getCols : Map[String, Col] = columns

  /**
   * A list of all columns in the dataset, as tuples of their variable name and the actual column.
   * Used internally for data formatting.
   * If an row number column exists, it will always be at index 0.
   * Any numeric columns containing only integers will be converted to string columns, with the values lacking any decimal places.
   */
  lazy val colTuples : List[(String, Col)] = {
    val raw = getCols.toList
    val cnames = raw.unzip._1
    // If rows are numbered, the row number should be in the first column
    val orderedcols = if(cnames.contains("#")) {
      val i = cnames.indexOf("#")
      val slices = raw splitAt i
      (slices._2.head :: slices._1) ::: slices._2.tail
    }else raw
    // NumCols containing only ints should be displayed without decimal places
    val orderedcolswithproperints = for(coltuple <- orderedcols) yield {
      val rawcol = coltuple._2
      rawcol match {
        case NumCol(values) => {
          if((values.map(Helper.isInt).foldLeft(true)(_ && _)))
            (coltuple._1, new StrCol(values.map((x: Double) => x.toInt.toString)))
          else
            coltuple
        }
        case sc : StrCol    => coltuple
        case _              => throw new InvalidColException(coltuple._1)
      }
    }
    orderedcolswithproperints    
  }

  /**
   * Extract a single data column from the dataset.
   * @param cname The variable name of the requested data column.
   * @return The extracted data column.
   */
  def getCol(cname : String) : Col = {
    val result = columns getOrElse(cname, NotFound) 
    result match {
      case NotFound => throw new ColNotFoundException(cname)
      case StrCol(values) => result
      case NumCol(values) => result
    }
  }

  def apply(cname : String) = getCol(cname)

  /**
   * Extract a single numeric data column from the dataset.
   * @param cname The variable name of the requested data column.
   * @return The extracted data column.
   */
  def num(cname : String) : NumCol = getCol(cname) match {
    case nc : NumCol => nc
    case sc : StrCol => throw new NotNumericException(cname)
    case _           => throw new InvalidColException(cname)
  }

  /**
   * Extract a single non-numeric data column from the dataset.
   * @param cname The variable name of the requested data column.
   * @return The extracted data column.
   */
  def str(cname : String) : StrCol = getCol(cname) match {
    case sc : StrCol => sc
    case NumCol(v)   => new StrCol(for(x <- v) yield x.toString)
    case _           => throw new InvalidColException(cname)
  }

  /**
   * Adds a new data column to the dataset. Because datasets are immutable, this returns a new dataset.
   * @param cname The variable name for the new data column
   * @param col The new data column
   * @return The new dataset, which includes the additional column
   */
  def +(cname : String, col : Col) = {
    val newcol = (cname, col)
    new Dataset(columns + newcol)
  }

  /**
   * Removes a data column from the dataset. Because datasets are immutable, this returns a new dataset.
   * @param cname The variable name of the unwanted data column
   * @return The new dataset, which omits the specified column
   */
  def -(cname : String) = new Dataset(columns - cname)
  
  /**
   * Removes multiple data columns from the dataset. Because datasets are immutable, this returns a new dataset.
   * @param cname The variable names of the unwanted data columns
   * @return The new dataset, which omits the specified columns
   */
  def --(cnames : String*) = new Dataset(columns -- cnames)

  /**
   * Creates a subset of the dataset, including only the specified data columns.
   * @param cname The variable names of the desired data columns
   * @return The new dataset, which includes only the specified columns
   */
  def $(cnames : String*) : Dataset = {
    val cols = for(cname <- cnames.toList) yield getCol(cname)
    new Dataset((cnames zip cols).toMap)
  }

  /**
   * Adds a column of 1-based row numbers to the dataset.
   * Because datasets are immutable, this returns a new dataset.
   */
  def withRowNumbers = {
    val unmapped = columns.toList
    val rownumbers = new NumCol(
      (for(i <- 1 to unmapped.head._2.length) yield i.toDouble).toVector
    )
    new Dataset((("#", rownumbers) :: unmapped).toMap)
  }

  /**
   * Creates a subset of the dataset, including only those rows for which a specified data column has a specified value.
   * @param cname The variable names of the data column by which to filter
   * @param value The value which the specified column must have for a row to remain in the dataset.
   * @return The new dataset, which includes only rows matching the filter
   */
  def filter(cname : String, value : String) : Dataset = {
    val col = getCol(cname).asStrList.toVector
    val rownums = for(i <- 0 until col.length if col(i) == value) yield i
    val newmap = for((n, c) <- columns) yield {
      val newc = c match {
        case NumCol(v) => {
          val newvec = (for(rn <- rownums) yield v(rn)).toVector
          new NumCol(newvec)
        }
        case StrCol(v) => {
          val newvec = (for(rn <- rownums) yield v(rn)).toVector
          new StrCol(newvec)
        }
        case _         => throw new InvalidColException(cname)
      }
      (n, newc)
    }
    new Dataset(newmap)
  }

  /**
   * Creates a subset of the dataset, including only those rows for which a specified numeric data column has a specified value.
   * @param cname The variable names of the numeric data column by which to filter
   * @param value The value which the specified column must have for a row to remain in the dataset.
   * @return The new dataset, which includes only rows matching the filter
   */
  def filter(cname : String, value : Double) : Dataset = getCol(cname) match {
    case NumCol(col) => {
      val rownums = for(i <- 0 until col.length if col(i) == value) yield i
      val newmap = for((n, c) <- columns) yield {
        val newc = c match {
          case NumCol(v) => {
            val newvec = (for(rn <- rownums) yield v(rn)).toVector
            new NumCol(newvec)
          }
          case StrCol(v) => {
            val newvec = (for(rn <- rownums) yield v(rn)).toVector
            new StrCol(newvec)
          }
          case _         => throw new InvalidColException(cname)
        }
        (n, newc)
      }
      new Dataset(newmap)
    }
    case StrCol(col) => throw new NotNumericException(cname)
    case _           => throw new InvalidColException(cname)
  }

  def aggrEven(cname : String, firstCeiling : Double, breadth : Double, k : Int) = getCol(cname) match {
    
    case NumCol(col) => {
      // Generate classes
      val floor = new FloorBorderedClass(firstCeiling)
      val mid = for(i <- 0 until k - 2) 
                yield new DoubleBorderedClass(firstCeiling + i * breadth, firstCeiling + (i + 1) * breadth)
      val ceil = new CeilingBorderedClass(firstCeiling + (k - 2) * breadth)
      val classes = floor +: mid.toVector :+ ceil

      //Count number of elements falling into each class
      def loop(remainder : List[Double], values : Vector[Int]) : Vector[Int] = {
        def check(x : Double, i : Int, values : Vector[Int]) : Vector[Int] =
          if(classes(i).contains(x))
            values.updated(i, values(i) + 1)
          else
          check(x, i + 1, values)
        if(remainder.isEmpty) values
        else loop(remainder.tail, check(remainder.head, 0, values))
      }
      val values_init = for(c <- classes)
                        yield 0
      val values = loop(col.toList, values_init.toVector)

      //Generate new dataset
      val col_classes = new StrCol(
        for(c <- classes)
        yield c.desc
      )
      val col_values = new NumCol(values.map((x : Int) => x.toDouble))
      new Dataset(Map(
        cname -> col_classes,
        "n"   -> col_values
      ))
    }
    case StrCol(col) => throw new NotNumericException(cname)
    case _           => throw new InvalidColException(cname)
  }

  /*--- ROW-BASED FUNCTIONS ---*/

  /**
   * Transforms the dataset into a list of data rows.
   * Each row is represented as a sequence of String objects. Numeric values are transformed to their decimal representations.
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

  object tabhelper {
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
    def whitespace(word : String, coln : Int) = (for(i <- 0 to (longestPerCol(coln) - word.length) + 1) yield " ").mkString
    def dashes(coln : Int) = (for(i <- 0 to longestPerCol(coln) + 2) yield "-").mkString
  }

  /**
   * A simple table representation of the dataset, with values separated by whitespace.
   * Note that for large datasets, this can become unreadable because lines are broken at random places by the console.
   * In such a case, consider subsetting the dataset or removing columns before calling tab.
   */
  lazy val tab : String = {
    def catrow(row : List[String]) = (for(i <- 0 until row.length) yield {
      val word = row(i)
      word + tabhelper.whitespace(word, i)
    }).mkString
    val rows = getRows.map(catrow)
    "\n" + rows.head + "\n\n" + (for(row <- rows.tail) yield row + "\n").mkString
  }

  /**
   * A table representation of the dataset, with lines between values
   * Note that for large datasets, this can become unreadable because lines are broken at random places by the console.
   * In such a case, consider subsetting the dataset or removing columns before calling tab.
   */
  lazy val tabulate : String = {
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
    "\n" + rows.head + "\n" + (for(row <- rows.tail) yield row + "\n").mkString
  }

  /**
   * Sorts the dataset by one of its columns, using a chosen sort mode.
   * If the column is numeric, the dataset is sorted by the size of the column's values. 
   * Otherwise, sorting is done alphabetically.
   * Because datasets are immutable, this returns a new dataset.
   * @param cname The data column by which to sort the dataset. 
   * @param mode The sorting mode used. See [[seitzal.scalastat.SortMode]] for options.
   * @return The sorted dataset.
   */
  def sort(cname : String, mode : SortMode = SortMode.ASCENDING) : Dataset = {
    val heads = getRows.head
    val coln = (for(i <- 0 until heads.length if heads(i) == cname) yield i).head
    val rows = getRows.tail
    val newrows = getCol(cname) match {
      case NumCol(v) => rows.sortBy((row : List[String]) => row(coln).toDouble)
      case StrCol(v) => rows.sortBy((row : List[String]) => row(coln))
      case _         => throw new ColNotFoundException(cname)
    }
    mode match {
      case SortMode.ASCENDING => Dataset.fromRows(heads :: newrows)
      case SortMode.DESCENDING => Dataset.fromRows(heads :: (newrows.reverse))
    }
  }

}

/**
 * The companion object for the dataset type. Contains methods for creating dataset objects from various sources.
 */
object Dataset {

  /**
   * Attempts to read a dataset from a comma-seperated values file at the specified location.
   * The first line of the CSV is assumed to contain the variable names.
   * This method will only work with CSVs that represent a table, i.e. if all lines contain the same number of elements.
   * Columsn containing only numbers will automatically be parsed as numeric columns, while all other columns are parsed as string columns.
   * @param path The path to the CSV file.
   * @return A dataset object containing the data from the file.
   */
  def readCSV(path : String) : Dataset = {
    fromRows(FunCSV.decodeFile(path))

  }

  /**
   * Attempts to create a dataset from a list of rows, each row being itself a list of string-formatted values.
   * The first sublist is assumed to contain the variable names.
   * This method will only work with list structures that represent a table, i.e. if all lines contain the same number of elements.
   * Columsn containing only numbers will automatically be parsed as numeric columns, while all other columns are parsed as string columns.
   * @param data A list of rows, each row being itself a list of string-formatted values
   * @return A dataset object containing the data from the list.
   */
  def fromRows(raw : List[List[String]]) : Dataset = {
    // Datasets may not include multiple columns of the same name
    if(!Helper.nodoubles(raw.head)) throw new NameNotUniqueException()

    val rawcols = for(i <- 0 until raw.tail.head.length) yield(
      for(row <- raw.tail) yield row(i)
    )
    val cols = for(rawcol <- rawcols) yield parseCol(rawcol)
    new Dataset((raw.head zip cols).toMap)
  }

  /**
   * (Used internally)
   * Infers the datatype of a column from its values, and returns either a NumCol if all values are numeric, or a StrCol if otherwise.
   */
  private def parseCol(raw : List[String]) : Col = {
    val numtry = Try{for (entry <- raw) yield entry.toDouble}.toOption
    numtry match {
      case Some(x) => new NumCol(x.toVector)
      case None    => new StrCol(raw.toVector)
    }
  }
}
