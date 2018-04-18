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
  object Correl extends Tag("Correl")

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
    lazy val data3 = Dataset.readCSV("testdata/distri3.csv")
  }

  trait EnvAggr {
    lazy val ratings = Dataset.readCSV("testdata/ratings.csv")
  }

  trait EnvCorrel {
    lazy val data1 = Dataset.readCSV("testdata/correl1.csv")
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

  test("Robin Hood index", Distri) {
    val col = new NumCol(Vector(2.5,0.5,0.5,0.5))
    assert(col.rhi == 1.5 / 4)
  }

  test("Concentration rate", Distri) {
    val col = new NumCol(Vector(30, 50, 10, 12, 100, 7, 8))
    assert(col.crate(3) == 0.8294931)
  }

  test("Herfindahl index 1", Distri) {
    new EnvAggr {
      assert(ratings.num("Rating").herfindahl == 0.0469612)
    }
  }

  test("Herfindahl index 2", Distri) {
    new EnvDistri {
      assert(data3.num("Neuzulassungen").herfindahl == 0.228056)
    }
  }

  test("Fixed-breadth single column aggregation", Aggr) {
    new EnvAggr {
      val aggr = ratings.aggrEven("Rating", 1, .5, 10).withRowNumbers
      println((aggr + ("u", aggr.num("n").relfreq)).tabulate)
    }
  }

  test("Covariance", Correl) {
    new EnvCorrel {
      val covxy = data1.cov("X", "Y")
      assert(Helper.roundTo(covxy, 2) == 7.94)
    }
  }

  test("Covariance error case: Different column lengths", Correl) {
    val brokendata = new Dataset(
      Map(
        "X" -> new NumCol(Vector(2,3.0,5)),
        "Y" -> new NumCol(Vector(3,4,4.5,10))
      )
    )
    assertThrows[ColLengthException] {
      val covxy = brokendata.num("X") cov brokendata.num("Y")
    }
  }
  
}
