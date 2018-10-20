package eu.seitzal.insight

/**
 * Represents a matrix containing correlation measures
 * (e.g. Pearson's r) between a number of variables.
 */
class CorrelationMatrix(val vnames : Vector[String], 
    val values : Vector[Vector[Double]]) {

  /** 
   * Returns the correlation between two variables.
   * @param vname1 The first variable
   * @param vname2 The second variable
   */
  def apply(vname1 : String, vname2 : String) = 
    values(vnames.indexOf(vname1))(vnames.indexOf(vname2))

  /**
   * Returns a simple tabular representation of the correlation matrix.
   */
  override def toString = {
    
    def compare(s1 : String, s2 : String) : String =
      if (s1.length > s2.length) s1
      else s2

    val padLength = 
      for (col <- 0 until vnames.length) 
      yield ((vnames(col) :: values(col).map(_.toString).toList)
        .foldLeft("")(compare)).length

    val padLengthVname = (vnames.foldLeft("")(compare)).length

    def pad(s : String, col : Int) =
      s + (
        for (i <- 0 until (padLength(col) - s.length + 2)) yield " "
      ).mkString

    def padVname(vname : String) =
      vname + (
        for (i <- 0 until (padLengthVname - vname.length + 2)) yield " "
      ).mkString

    val headline = 
      padVname("") + " " + (
        for (i <- 0 until vnames.length) yield pad(vnames(i), i)
      ).mkString + "\n\n"

    val body = for (row <- 0 until vnames.length) yield {
      padVname(vnames(row)) + " " + (
        for (col <- 0 until vnames.length)
        yield pad((values(row)(col)).toString, col)
      ).mkString + "\n"
    }

    "\n" + headline + body.mkString
  }

}