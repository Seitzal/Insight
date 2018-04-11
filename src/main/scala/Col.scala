package seitzal.scalastat

/**
 * A column of data. Can be numeric ([[seitzal.scalastat.NumCol]]) or non-numeric ([[seitzal.scalastat.StrCol]])
 */
trait Col {

  /**
   * The number of values contained in the column
   */
  val length : Int

  /**
   * A list of string objects representing the values contained in the column
   */
  val asStrList : List[String]

  /**
   * A collection of measures and aggregations depending on the data type
   */
  val info : String

}

/**
 * Pseudo-object representing a nonexisting column
 */
object NotFound extends Col {
  val length = 0
  val asStrList = Nil
  val info = "Column not found."
  override def toString = "Column not found."
}