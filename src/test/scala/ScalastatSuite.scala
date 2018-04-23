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
  object MissingVals extends Tag("MissingVals")

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
    lazy val data2 = Dataset.readCSV("testdata/correl2.csv")
  }

  trait EnvMissingVals {
    lazy val data = Dataset.readCSV("testdata/missingvals.csv")
  }

  //Tests
  test("Cumulative absolute frequencies", Freq) {
    new EnvFreq {
      val cf = freq.num("Anzahl").cabsfreq
      assert(
        cf.values(cf.values.length - 1).get == freq.num("Anzahl").sum
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
    val col = NumCol(2.5,0.5,0.5,0.5)
    assert(col.rhi == 1.5 / 4)
  }

  test("Concentration rate", Distri) {
    val col = NumCol(30, 50, 10, 12, 100, 7, 8)
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
        "X" -> NumCol(2,3.0,5),
        "Y" -> NumCol(3,4,4.5,10)
      )
    )
    assertThrows[ColLengthException] {
      val covxy = brokendata.cov("X", "Y")
    }
  }

  test("Pearson's r", Correl) {
    new EnvCorrel {
      val r = data1.pearson("X", "Y")
      assert(Helper.roundTo(r, 3) == 0.856)
    }
  }

  test("Partial correlation test 1", Correl) {
    new EnvCorrel {
      assert(data1.num("X").pearsonPartial(data1.num("Y"), data1.num("Y")) == 0)
    }
  }

  test("Partial correlation test 2", Correl) {
    new EnvCorrel {
      val xyunderz = data1.pearsonPartial("X", "Y", "Z")
      assert(Helper.roundTo(xyunderz, 3) == 0.885)
    }
  }

  test("Simple linear regression", Correl) {
    new EnvCorrel {
      val reg = data2.ols("Footprint", "GDP/Capita")
      assert(Helper.roundTo(reg._1, 3) == 1.199)
      assert(Helper.roundTo(reg._2, 6) == 0.000148)
    }
  }

  test("R-squared", Correl) {
    new EnvCorrel {
      val rsquared = math.pow(data2.pearson("Footprint", "GDP/Capita"), 2)
      assert(Helper.roundTo(rsquared, 3) == 0.349)
    }
  }
 
  test("Missing Values", MissingVals) {
    new EnvMissingVals {
      val a = data.num("A")
      val b = data.num("B")
      val c = data.num("C")
      println(data.withRowNumbers.tab)
    }
  }

  test("Common existing length", MissingVals) {
    new EnvMissingVals {
      assert(data.num("B").commonExistingLength(data.num("C")) == 3)
    }
  }

  test("Common existing values", MissingVals) {
    new EnvMissingVals {
      assert(data.num("B").commonExistingValues(data.num("C")) ==
        (Vector(3.5, 3.0, 3.0), Vector(12.0, 0.4, 2.4)))
    }
  }
}
