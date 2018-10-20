package eu.seitzal.insight.plot

import java.awt._
import eu.seitzal.insight._

/**
 *  Draw a scatter plot on a Swing/AWT frame.
 *  This is useful for visualising correlation patterns between variables,
 *  and deciding which methods to use for further analysis.
 */
class ScatterPlot(xcol : NumSeries, ycol : NumSeries, xname : String, yname : String, symbol : (Graphics, Int, Int) => Unit) extends Plot {
  
  // TODO: Make these changeable parameters
  override val width = 600
  override val height = 600
  override val title = "Scatterplot: " + xname + " => " + yname
  
  // Cut blanks on either variable
  protected val cleancols = xcol commonExisting ycol
  protected val cleandata = xcol commonExistingValues ycol
  protected val xs = cleandata._1
  protected val ys = cleandata._2

  // Calculate boundaries
  protected val rminx = cleancols._1.min
  protected val xmin = 
    if (rminx <= 0) rminx
    else if(rminx < 1) -rminx
    else -1

  protected val rminy = cleancols._2.min
  protected val ymin = 
    if (rminy <= 0) rminy
    else if(rminy < 1) -rminy
    else -1

  protected val xmax = cleancols._1.max
  protected val ymax = cleancols._2.max

  protected val xrange = xmax - xmin
  protected val yrange = ymax - ymin

  // Calculate the scale
  protected val xscale = 550 / (xrange) 
  protected val yscale = 550 / (yrange)

  // Calculate offsets to avoid negative coordinates
  protected val xoffset = (0 - xmin) * xscale
  protected val yoffset = (0 + ymin) * yscale

  // Functions to calculate physical screen coordinates for any point in the virtual data plane
  protected def calcx (x : Double) : Int = (25 + xoffset + x * xscale).toInt
  protected def calcy (y : Double) : Int = (575 + yoffset - y * yscale).toInt

  // Draw the plot
  override protected def draw(g : Graphics) {

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

/**
 * Companion object of the ScatterPlot class.
 * Contains methods for succinct plot creation.
 */
object ScatterPlot {
  
  /** 
   * Creates a scatterplot from two variables contained within the same dataset.
   * This can be called in the following short syntax:
   * <pre><code>ScatterPlot(dataset, "first variable", "second variable")</code></pre>
   * @param symbol The method to draw a data point at the screen coordinates (X, Y). Presets can be found in [[eu.seitzal.insight.plot.Symbol]]. Default is a small x.
   */
  def apply(dataset : DataFrame, cname1 : String, cname2 : String, symbol : (Graphics, Int, Int) => Unit = Symbol.X_SMALL) = 
    new ScatterPlot(dataset.num(cname1), dataset.num(cname2), cname1, cname2, symbol)
} 