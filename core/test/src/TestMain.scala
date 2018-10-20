package eu.seitzal.test.insight

import org.scalatest._
import org.scalactic.TolerantNumerics
import eu.seitzal.insight._

class TestMain extends FunSuite with Tags {

  implicit private val doubleEq = TolerantNumerics.tolerantDoubleEquality(1e-4f)

  // Environments
  trait EnvQOG {
    lazy val qog = read.csv("testdata/qog_bas_ts_jan18.csv")
  }

  trait EnvFreq {
    lazy val freq = read.csv("testdata/freq.csv")
  }

  trait EnvDistri {
    lazy val data1 = read.csv("testdata/distri1.csv")
    lazy val data2 = read.csv("testdata/distri2.csv")
    lazy val data3 = read.csv("testdata/distri3.csv")
  }

  trait EnvAggr {
    lazy val ratings = read.csv("testdata/ratings.csv")
  }

  trait EnvCorrel {
    lazy val data1 = read.csv("testdata/correl1.csv")
    lazy val data2 = read.csv("testdata/correl2.csv")
  }

  trait EnvMissingVals {
    lazy val data = read.csv("testdata/missingvals.csv")
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

  test("filtered subset", FSS, NoJenkins, Slow) {
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

  test("sorted subset", FSS, NoJenkins, Slow) {
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

  test("tab function", Out, NoJenkins) {
    println(read.csv("testdata/test.csv").withRowNumbers.tab)
  }

  test("tabulate function", Out, NoJenkins) {
    println(read.csv("testdata/test.csv").withRowNumbers.tabulate)
  }

  test("tabulate function with row numbers read from file", Out, NoJenkins) {
    println(read.csv("testdata/distri1.csv").tabulate)
  }

  test("Gini index for unaggregated data", Distri) {
    new EnvDistri {
      assert((data1 num("Umsatz") gini) === 0.432)
      assert((data1 num("Umsatz") ngini) === 0.48)
    }
  }

  test("Gini index for unaggregated relative frequencies", Distri) {
    new EnvDistri {
      assert(data1.num("Umsatz").relfreq.gini === 0.432)
      assert(data1.num("Umsatz").relfreq.ngini === 0.48)
    }
  }

  test("Gini index for aggregated data", Distri) {
    new EnvDistri {
      assert(data2.num("Prozent der Arbeitnehmer").cgini == 0.424) 
    }
  }

  test("Robin Hood index", Distri) {
    val xs = NumSeries(2.5,0.5,0.5,0.5)
    assert(xs.rhi == 1.5 / 4)
  }

  test("Concentration rate", Distri) {
    val xs = NumSeries(30, 50, 10, 12, 100, 7, 8)
    assert(xs.crate(3) === 0.8294931)
  }

  test("Herfindahl index 1", Distri) {
    new EnvAggr {
      assert(ratings.num("Rating").herfindahl === 0.0469612)
    }
  }

  test("Herfindahl index 2", Distri) {
    new EnvDistri {
      assert(data3.num("Neuzulassungen").herfindahl == 0.228056)
    }
  }

  test("Skewness", Distri) {
    new EnvAggr {
      assert(Helper.roundTo(ratings.num("Rating").skewness, 3) == -0.539)
    }
  }

  test("Kurtosis", Distri) {
    new EnvAggr {
      assert(Helper.roundTo(ratings.num("Rating").kurtosis, 3) == -0.292)
    }
  }

  test("Fixed-width single column aggregation", Aggr, NoJenkins) {
    new EnvAggr {
      val aggr = ratings.aggregateByValue("Rating", 1, .5, 10).withRowNumbers
      println((aggr + ("u", aggr.num("n").relfreq)).tabulate)
    }
  }

  test("Covariance", Correl) {
    new EnvCorrel {
      val covxy = data1.cov("X", "Y")
      assert(Helper.roundTo(covxy, 2) == 7.94)
    }
  }

  test("Pearson's r", Correl) {
    new EnvCorrel {
      val r = data1.pearson("X", "Y")
      assert(Helper.roundTo(r, 3) == 0.856)
    }
  }

  test("Pearson's r for large dataset", Correl, NoJenkins, Slow) {
    new EnvQOG {
      val r = qog.pearson("rsf_pfi", "wef_ptp")
      assert(Helper.roundTo(r, 4) == -0.0799)
      val r2 = qog.pearson("wdi_gdpcapcon2010", "undp_hdi")
      assert(Helper.roundTo(r2, 3) == 0.69)
    }
  }

  test("Partial correlation", Correl) {
    new EnvCorrel {
      val xyunderz = data1.pearsonPartial("X", "Y", "Z")
      assert(Helper.roundTo(xyunderz, 3) == 0.885)
    }
  }

  test("Simple linear regression", Regression) {
    new EnvCorrel {
      val reg = regression.SimpleOLS.build(data2, "GDP/Capita",  "Footprint")
      assert(Helper.roundTo(reg.intersect, 3) == 1.199)
      assert(Helper.roundTo(reg.slope, 6) == 0.000148)
      assert(Helper.roundTo(reg.estimate(8000), 3) == 2.383)
    }
  }

  test("R-squared for simple linear regression", Regression) {
    new EnvCorrel {
      val reg = regression.SimpleOLS.build(data2, "GDP/Capita",  "Footprint")
      assert(Helper.roundTo(reg.rsq, 3) == 0.349)
    }
  }
 
  test("Missing Values", MissingVals, NoJenkins) {
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

  test("Pearson Matrix", CorrelMat, Slow, NoJenkins) {
    new EnvQOG {
      val filtered = qog $ ("rsf_pfi", "wef_ptp", "wdi_gdpcapcon2010", "undp_hdi", "gle_pop")
      println(filtered.pearsonMatrix)
    }
  }

  test("ntiles", Aggr, Slow, NoJenkins) {
    new EnvQOG {
      val filtered = qog $ ("year", "cname", "undp_hdi") filter ("year", 2010)
      assert(filtered.quantile("undp_hdi", 10, 0.85) == 9)
    }
  }
}
