package eu.seitzal.test.insight

import org.scalatest._
import eu.seitzal.insight._

class TextExport extends FunSuite with Tags {

  test("Html export, small dataset", Export) {
    val data = read.csv("testdata/test.csv")
    write.html(data, "test-outputs/test1.html")
  }

  test("Html export, medium dataset", Export) {
    val data = read.csv("testdata/gii.csv")
    write.html(data, "test-outputs/test2.html")
  }

  test("Html export, larger dataset", Export, Slow) {
    val data = read.csv("testdata/qog_bas_ts_jan18.csv")
    val filtered = data.filter("year", 2012)
    write.html(filtered.withRowNumbers, "test-outputs/test3.html")
  }

}