package eu.seitzal.insight.write

import eu.seitzal.insight._
import java.io.{PrintWriter, File, IOException}

class CSVWriter(path : String) {

  def writeDataFrame(df : DataFrame) = {
    val f = new File(path)
    val pw = new PrintWriter(f)
    val ncols = df.columns.length

    if (ncols <= 1)
      throw new Error("Can't export empty dataframe")
    
    // Write headers
    for (i <- 0 until ncols - 1) {
      pw.print("\"" + df.columns(i)._1 + "\",")
    }
    pw.println("\"" + df.columns(ncols - 1)._1 + "\"")

    // Write data
    for (i <- 0 until df.columns(0)._2.length) {
      for (c <- 0 until ncols - 1) df.columns(c)._2 match {
        case nc : NumSeries => nc(i) match {
          case None => 
            pw.print("NA,")
          case x : Option[Double] => {
            if (Helper.isInt(x))
              pw.print(Helper.toIntString(x) + ",")
            else 
              pw.print(x.getOrElse(0.0).toString + ",")
          }
        }
        case c : StrSeries =>
          pw.print("\"" + c(i) + "\",")
        case _ => throw new InvalidSeriesException(df.columns(c)._1)
      }
      df.columns(ncols - 1)._2 match {
        case nc : NumSeries => nc(i) match {
          case None => 
            pw.println("NA")
          case x : Option[Double] => {
            if (Helper.isInt(x)) 
              pw.println(Helper.toIntString(x))
            else 
              pw.println(x.getOrElse(0.0).toString)
          }
        }
        case c : StrSeries =>
          pw.println("\"" + c(i) + "\"")
        case _ => throw new InvalidSeriesException(df.columns(ncols - 1)._1)
      }
    }

    pw.close()
  }

}