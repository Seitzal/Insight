package eu.seitzal.insight.plot

import java.awt._
import java.awt.event._
import javax.swing._

/**
 * Abstract superclass for all plot types.
 */
abstract class Plot {

  /**
   * The screen width in pixels that the plot takes, window borders excluded.
   */
  val width : Int

  /**
   * The screen height in pixels that the plot takes, window borders excluded.
   */
  val height : Int

  /**
   * A brief description of the plot. Shown in the window title bar.
   */
  val title : String

  protected def draw (g : Graphics)

  /**
   * Create a javax.swing.JFrame and draw the plot on it.
   * Dev Note: No swing environment is required before this is called. 
   * Calling this in a non-graphical environment (e.g. Jenkins, WSL bash) will cause a JVM crash.
   */
  def show {
    val panel = new JPanel {
      override def paintComponent(g : Graphics) { draw(g) }
    }
    panel.setPreferredSize(new Dimension(width, height))
    val frame = new JFrame(title)
    frame.add(panel)
    frame.pack
    frame.setVisible(true)
  }
}

/**
 * Dummy class for fast creation of simple, custom plots.
 * For more complex applications, it is preferred to create a new child class of [[eu.seitzal.insight.plot.Plot]]
 */
class CustomPlot (p_width : Int, p_height : Int, p_title : String, drawer : (Graphics) => Unit) extends Plot {
  override val width = p_width
  override val height = p_height
  override val title = p_title
  override def draw (g : Graphics) {
    drawer(g)
  }
}