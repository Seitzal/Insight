package eu.seitzal.insight

/**
 * A series of data. 
 */
trait Series {

  /**
   * The number of values contained in the series
   */
  val length : Int

  /**
   * A list of string objects representing the values contained in the series
   */
  val asStrList : List[String]

  /**
   * A collection of measures and aggregations depending on the data type
   */
  val info : String

}

/**
 * Pseudo-object representing a nonexisting series
 */
object NotFound extends Series {
  val length = 0
  val asStrList = Nil
  val info = "Column not found."
  override def toString = "Column not found."
}