package seitzal.scalastat.test

import org.scalatest._
import seitzal.scalastat._

class ScalastatSuite extends FunSuite {

  // Tags
  object FSS extends Tag("FSS")
  object Freq extends Tag("Freq")
  object Out extends Tag("Out")
  object Never extends Tag("Never")
  object Distri extends Tag("Distri")
  object Aggr extends Tag("Aggr")

  // Environments
  trait EnvQOG {
    lazy val qog = Dataset.readCSV("testdata/qog_bas_ts_jan18.csv")
  }

  trait EnvFreq {
    lazy val freq = Dataset.readCSV("testdata/freq.csv")
  }

  trait EnvDistri {
    lazy val data1 = Dataset.readCSV("testdata/distri1.csv")
    lazy val data2 = Dataset.readCSV("testdata/distri2.csv")
  }

  trait EnvAggr {
    lazy val ratings = Dataset.readCSV("testdata/ratings.csv")
  }

  //Tests
  test("Cumulative absolute frequencies", Freq) {
    new EnvFreq {
      val cf = freq.num("Anzahl").cabsfreq
      assert(
        cf.values(cf.values.length - 1) == freq.num("Anzahl").sum
      )
    }
  }

  test("filtered subset", FSS) {
    new EnvQOG {
      println (
        qog
        .$("ccode", "cname" , "year" , "ccodealp" , "cname_year", "ccodealp_year", "ccodecow", "ccodewb", "version", "ajr_settmort")
        .filter("year", 2012)
        .filter("ccodealp", "GBR")
        .withRowNumbers
        .tab
      )
    }
  }

  test("sorted subset", FSS) {
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

  test("tab function", Out) {
    println(Dataset.readCSV("testdata/test.csv").withRowNumbers.tab)
  }

  test("tabulate function", Out) {
    println(Dataset.readCSV("testdata/test.csv").withRowNumbers.tabulate)
  }

  test("tabulate function with row numbers read from file", Out) {
    println(Dataset.readCSV("testdata/distri1.csv").tabulate)
  }

  test("Gini index for unaggregated data", Distri) {
    new EnvDistri {
      assert((data1 num("Umsatz") gini) == 0.432)
      assert((data1 num("Umsatz") ngini) == 0.48)
    }
  }

  test("Gini index for unaggregated relative frequencies", Distri) {
    new EnvDistri {
      assert(data1.num("Umsatz").relfreq.gini == 0.432)
      assert(data1.num("Umsatz").relfreq.ngini == 0.48)
    }
  }

  test("Gini index for aggregated data", Distri) {
    new EnvDistri {
      assert(data2.num("Prozent der Arbeitnehmer").cgini == 0.424) 
    }
  }

  test("Fixed-breadth single column aggregation", Aggr) {
    new EnvAggr {
      val aggr = ratings.aggrEven("Rating", 1, .5, 10).withRowNumbers
      println((aggr + ("u", aggr.num("n").relfreq)).tabulate)
    }
  }
  
}
