package seitzal.scalastat.plot

import java.awt._
import seitzal.scalastat._

/**
 *  Draw a scatter plot on a Swing/AWT frame.
 *  This is useful for visualising correlation patterns between variables,
 *  and deciding which methods to use for further analysis.
 */
class ScatterPlot(xcol : NumCol, ycol : NumCol, xname : String, yname : String, symbol : (Graphics, Int, Int) => Unit) extends Plot {
  
  // TODO: Make these changeable parameters
  override val width = 600
  override val height = 600
  override val title = "Scatterplot: " + xname + " => " + yname
  
  // Cut blanks on either variable
  val cleancols = xcol commonExistingCols ycol
  val cleandata = xcol commonExistingValues ycol
  val xs = cleandata._1
  val ys = cleandata._2

  // Calculate boundaries
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

  // Calculate the scale
  val xscale = 550 / (xrange) 
  val yscale = 550 / (yrange)

  // Calculate offsets to avoid negative coordinates
  val xoffset = (0 - xmin) * xscale
  val yoffset = (0 + ymin) * yscale

  // Functions to calculate physical screen coordinates for any point in the virtual data plane
  def calcx (x : Double) : Int = (25 + xoffset + x * xscale).toInt
  def calcy (y : Double) : Int = (575 + yoffset - y * yscale).toInt

  // Draw the plot
  override def draw(g : Graphics) {

    // Set brush colour to black
    g.setColor(Color.black)

    // Draw data points
    for (i <- 0 until xs.length) 
      symbol(g, calcx(xs(i)), calcy(ys(i)))

    // Symbol function might have changed the brush colour, so we want to ensure it's black again
    g.setColor(Color.black)

    // Draw X Axis
    g.drawLine(calcx(xmin), calcy(0), calcx(xmax), calcy(0))
    g.drawLine(calcx(0), calcy(0), calcx(xmax), calcy(0))
    g.drawLine(calcx(xmax), calcy(0), calcx(xmax) - 3, calcy(0) - 3)
    g.drawLine(calcx(xmax), calcy(0), calcx(xmax) - 3, calcy(0) + 3)
    
    // Draw Y Axis
    g.drawLine(calcx(0), calcy(ymin), calcx(0), calcy(ymax))
    g.drawLine(calcx(0), calcy(0), calcx(0), calcy(ymax))
    g.drawLine(calcx(0), calcy(ymax), calcx(0) - 3, calcy(ymax) + 3)
    g.drawLine(calcx(0), calcy(ymax), calcx(0) + 3, calcy(ymax) + 3)
    
    // Label Axes
    g.setColor(Color.blue)
    g.drawString(xname, calcx(xmax) - (30 + xname.length * 5), calcy(0) + 15)
    g.drawString(yname, calcx(0) + 5, calcy(ymax) + 30)
  }

}

// Factory object for succinct plot creation
object ScatterPlot {
  
  /** This can be called in the following short syntax:
   *  <pre><code>ScatterPlot(dataset, "first variable", "second variable")</code></pre>
   *  @param symbol The method to draw a data point at the screen coordinates (X, Y). Presets can be found in [[seitzal.scalastat.plot.Symbol]]. Default is a small x.
   */
  def apply(dataset : Dataset, cname1 : String, cname2 : String, symbol : (Graphics, Int, Int) => Unit = Symbol.X_SMALL) = 
    new ScatterPlot(dataset.num(cname1), dataset.num(cname2), cname1, cname2, symbol)
} 