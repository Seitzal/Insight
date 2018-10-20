package eu.seitzal.test.insight

import org.scalactic.TolerantNumerics
import eu.seitzal.insight._

trait EnvGen {
  implicit protected val doubleEq = TolerantNumerics.tolerantDoubleEquality(1e-4f)
}

object qogPreload {
  val qog = read.csv("testdata/qog_bas_ts_jan18.csv")
}

trait EnvQOG {
  def qog = qogPreload.qog
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