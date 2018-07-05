package eu.seitzal.scalastat.test

import org.scalatest._
import eu.seitzal.scalastat._

class ScalastatExportSuite extends FunSuite with Tags {
  test("Html export, small dataset", Export, NoJenkins) {
    val data = Dataset.readCSV("testdata/test.csv")
    new HtmlBuilder(data.withRowNumbers).export("test-outputs/test1.html")
  }

  test("Html export, medium dataset", Export, NoJenkins) {
    val data = Dataset.readCSV("testdata/gii.csv")
    new HtmlBuilder(data).export("test-outputs/test2.html")
  }

  test("Html export, larger dataset", Export, Slow, NoJenkins) {
    val data = Dataset.readCSV("testdata/qog_bas_ts_jan18.csv")
    println("read complete")
    val filtered = data.filter("year", 2012)
    println("filter complete")
    new HtmlBuilder(filtered.withRowNumbers).export("test-outputs/test3.html")
  }
}