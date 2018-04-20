package seitzal.scalastat

/**
 * Represents a data column containing not only numeric values.
 * Any numeric values contained are treated as non-numeric (use pattern matching to cast them back)
 */
case class StrCol (values : Vector[String]) extends Col {

  lazy val length = values.length
  lazy val asStrList = values.toList

  val info = ""

  override def toString = {
    val withtrailingcomma = (for(e <- values) yield "\"" + e + "\", ").mkString
    withtrailingcomma.substring(0, withtrailingcomma.length - 2)
  }

  def derive(func : String => Double) = new NumCol(values.map((x : String) => Option(Helper.round(func(x)))))
  def deriveStr(func : String => String) = new StrCol(values.map(func))

}
