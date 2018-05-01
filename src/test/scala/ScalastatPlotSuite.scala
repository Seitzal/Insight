package seitzal.scalastat.test

import org.scalatest._
import java.awt._
import java.awt.event._
import javax.swing._
import seitzal.scalastat._
import seitzal.scalastat.plot._

class ScalastatPlotSuite extends FunSuite {

  test("Test Plot 1", new Tag("TestPlot")) {
    def draw (g : Graphics) {
    g.fillRect(0, 0, 300, 300)
    g.setColor(Color.red)
    g.drawLine(0, 300, 300, 0)
    }
    val testplot = new CustomPlot(300, 300, "Test Plot 1", draw)
    testplot.show
  }

  test("Scatterplot 1", new Tag("Scatterplot")) {
    val data = Dataset.readCSV("testdata/correl1.csv")
    val scatter = new ScatterPlot(data.num("X"), data.num("Y"), "X", "Y")
    scatter.show
  }

  test("Scatterplot 2", new Tag("Scatterplot2")) {
    val data = Dataset.readCSV("testdata/qog_bas_ts_jan18.csv").filter("year", 2006)
    println("data read")
    val scatter = new ScatterPlot(data.num("wdi_gdpcapcon2010"), data.num("undp_hdi"), "GDP/Capita", "HDI")
    scatter.show
  }

}