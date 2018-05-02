package seitzal.scalastat.test

import org.scalatest._
import java.awt._
import java.awt.event._
import javax.swing._
import seitzal.scalastat._
import seitzal.scalastat.plot._

class ScalastatPlotSuite extends FunSuite {

  object Plot extends Tag("Plot")
  object Never extends Tag("Never")
  object Slow extends Tag("Slow")

  def cross(g : Graphics, x : Int, y : Int) {
    g.drawLine(x - 2, y + 2, x + 2, y - 2)
    g.drawLine(x - 2, y - 2, x + 2, y + 2)
  }

  test("Test Plot 1", new Tag("TestPlot"), Plot) {
    def draw (g : Graphics) {
    g.fillRect(0, 0, 300, 300)
    g.setColor(Color.red)
    g.drawLine(0, 300, 300, 0)
    }
    val testplot = new CustomPlot(300, 300, "Test Plot 1", draw)
    testplot.show
  }

  test("Scatterplot 1", new Tag("Scatterplot"), Plot) {
    val data = Dataset.readCSV("testdata/correl1.csv")
    ScatterPlot(data, "X", "Y").show
  }

  test("Scatterplot 2", new Tag("Scatterplot2"), Plot, Slow) {
    val data = Dataset.readCSV("testdata/qog_bas_ts_jan18.csv")
    ScatterPlot(data, "wdi_gdpcapcon2010", "undp_hdi", Symbol.DOT).show
  }

}