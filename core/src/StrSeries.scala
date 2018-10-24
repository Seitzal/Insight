package eu.seitzal.insight

import scala.collection.parallel.immutable.ParVector

/**
 * Represents a data series containing not only numeric values.
 * Any numeric values contained are treated as non-numeric
 * (use pattern matching to cast them back)
 */
case class StrSeries (values : ParVector[String]) extends Series {

  lazy val length = values.length
  lazy val asStrList = values.toList

  val info = ""

  override def toString = {
    val withtrailingcomma = (for(e <- values) yield "\"" + e + "\", ").mkString
    withtrailingcomma.substring(0, withtrailingcomma.length - 2)
  }

  def derive(func : String => Double) = 
    new NumSeries(values.map((x : String) => Option(Helper.round(func(x)))))
  def deriveStr(func : String => String) = 
    new StrSeries(values.map(func))

  def apply(index : Int) = values(index)

}
