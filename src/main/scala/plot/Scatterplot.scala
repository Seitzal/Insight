package seitzal.scalastat.plot

import java.awt._
import seitzal.scalastat._

class ScatterPlot(xcol : NumCol, ycol : NumCol, xname : String, yname : String) extends Plot {
  override val width = 600
  override val height = 600
  override val title = "Scatterplot: " + xname + " => " + yname

  val cleancols = xcol commonExistingCols ycol
  val cleandata = xcol commonExistingValues ycol

  val xs = cleandata._1
  val ys = cleandata._2

  val rminx = cleancols._1.min
  val xmin = 
    if (rminx <= 0) rminx
    else if(rminx < 1) -rminx
    else -1

  val rminy = cleancols._2.min
  val ymin = 
    if (rminy <= 0) rminy
    else if(rminy < 1) -rminy
    else -1

  val xmax = cleancols._1.max
  val ymax = cleancols._2.max

  val xrange = xmax - xmin
  val yrange = ymax - ymin

  val xscale = 550 / (xrange) 
  val yscale = 550 / (yrange)

  val xoffset = (0 - xmin) * xscale
  val yoffset = (0 + ymin) * yscale

  def calcx (x : Double) : Int = (25 + xoffset + x * xscale).toInt
  def calcy (y : Double) : Int = (575 + yoffset - y * yscale).toInt

  override def draw(g : Graphics) {
    def drawCross(x : Int, y : Int, r : Int = 3) {
      g.drawLine(x - r, y + r, x + r, y - r)
      g.drawLine(x - r, y - r, x + r, y + r)
    }
    g.drawLine(calcx(xmin), calcy(0), calcx(xmax), calcy(0))
    g.drawLine(calcx(0), calcy(0), calcx(xmax), calcy(0))
    g.drawLine(calcx(0), calcy(ymin), calcx(0), calcy(ymax))
    g.drawLine(calcx(0), calcy(0), calcx(0), calcy(ymax))
    for (i <- 0 until xs.length) 
      drawCross(calcx(xs(i)), calcy(ys(i)))
  }

}  