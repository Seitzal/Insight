package eu.seitzal.test.insight

import org.scalatest._

trait Tags {
  // Tags
  object FSS extends Tag("FSS")
  object Freq extends Tag("Freq")
  object Out extends Tag("Out")
  object Distri extends Tag("Distri")
  object Aggr extends Tag("Aggr")
  object Correl extends Tag("Correl")
  object Regression extends Tag("Regression")
  object CorrelMat extends Tag("CorrelMat")
  object MissingVals extends Tag("MissingVals")
  object Export extends Tag("Export")
  object Lazy extends Tag("Lazy")

  // Utility tags
  object NoJenkins extends Tag("NoJenkins")
  object HasOutput extends Tag("HasOutput")
  object Slow extends Tag("Slow")
}