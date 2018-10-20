package eu.seitzal.test.insight

import org.scalatest._
import eu.seitzal.insight._

class TextExport extends FunSuite with Tags {

  test("Html export, small dataset", Export) {
    val data = read.csv("testdata/test.csv")
    new HtmlBuilder(data.withRowNumbers).export("test-outputs/test1.html")
  }

  test("Html export, medium dataset", Export) {
    val data = read.csv("testdata/gii.csv")
    new HtmlBuilder(data).export("test-outputs/test2.html")
  }

  test("Html export, larger dataset", Export, Slow, NoJenkins) {
    val data = read.csv("testdata/qog_bas_ts_jan18.csv")
    val filtered = data.filter("year", 2012)
    new HtmlBuilder(filtered.withRowNumbers).export("test-outputs/test3.html")
  }
}