package eu.seitzal.test.insight

import org.scalatest._
import eu.seitzal.insight._

class TestCLI extends FunSuite with Tags with EnvGen {

   test("Fixed-width single column aggregation", Aggr, NoJenkins, HasOutput) {
    new EnvAggr {
      val aggr = ratings.aggregateByValue("Rating", 1, .5, 10).withRowNumbers
      println((aggr + ("u", aggr.num("n").relfreq)).tabulate)
    }
  }

  test("filtered subset", FSS, NoJenkins, Slow, HasOutput) {
    new EnvQOG {
      println (
        qog
        .$("ccode", "cname" , "year" , "ccodealp" , "cname_year", "ccodealp_year", "ccodecow", "ccodewb", "version", "ajr_settmort")
        .filter("year", 2012)
        .filterS("ccodealp", "GBR")
        .withRowNumbers
        .tab
      )
    }
  }

  test("sorted subset", FSS, NoJenkins, Slow, HasOutput) {
    new EnvQOG {
      println (
        qog
        .$("ccode", "cname", "year", "ccodealp")
        .sort("year", SortMode.DESCENDING)
        .withRowNumbers
        .tabulate
      )
    }
  }

  test("tab function", Out, NoJenkins, HasOutput) {
    println(read.csv("testdata/test.csv").withRowNumbers.tab)
  }

  test("tabulate function", Out, NoJenkins, HasOutput) {
    println(read.csv("testdata/test.csv").withRowNumbers.tabulate)
  }

  test("tabulate function with row numbers read from file", Out, NoJenkins, HasOutput) {
    println(read.csv("testdata/distri1.csv").tabulate)
  }

  test("Pearson Matrix", CorrelMat, Slow, NoJenkins, HasOutput) {
    new EnvQOG {
      val filtered = qog $ ("rsf_pfi", "wef_ptp", "wdi_gdpcapcon2010", "undp_hdi", "gle_pop")
      println(filtered.pearsonMatrix)
    }
  }

  test("Missing Values", MissingVals, NoJenkins, HasOutput) {
    new EnvMissingVals {
      val a = data.num("A")
      val b = data.num("B")
      val c = data.num("C")
      println(data.withRowNumbers.tab)
    }
  }

  test("Contingency table print", Freq, NoJenkins, HasOutput) {
    val data = read.csv("testdata/freq2.csv")
    println(new ContingencyTable(data("A"), data("B")))
  }
}