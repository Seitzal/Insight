package eu.seitzal.scalastat.plot

import java.awt.Graphics

/**
 *  Contains predefined drawer methods for commonly used data point visualisations, such as circles, dots or crosses.
 */
object Symbol {
  def X_SMALL(g : Graphics, x : Int, y : Int) {
    g.drawLine(x - 2, y + 2, x + 2, y - 2)
    g.drawLine(x - 2, y - 2, x + 2, y + 2)
  }

  def X_MEDIUM(g : Graphics, x : Int, y : Int) {
    g.drawLine(x - 3, y + 3, x + 3, y - 3)
    g.drawLine(x - 3, y - 3, x + 3, y + 3)
  }

  def X_LARGE(g : Graphics, x : Int, y : Int) {
    g.drawLine(x - 4, y + 4, x + 4, y - 4)
    g.drawLine(x - 4, y - 4, x + 4, y + 4)
  }

  def DOT(g : Graphics, x : Int, y : Int) {
    g.drawLine(x, y, x, y)
  }

  def CIRCLE_SMALL(g : Graphics, x : Int, y : Int) {
    g.drawOval(x - 2, y - 2, 4, 4)
  }

  def CIRCLE_MEDIUM(g : Graphics, x : Int, y : Int) {
    g.drawOval(x - 3, y - 3, 6, 6)
  }

  def CIRCLE_LARGE(g : Graphics, x : Int, y : Int) {
    g.drawOval(x - 4, y - 4, 8, 8)
  } 
}