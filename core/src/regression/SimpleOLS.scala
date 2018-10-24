package eu.seitzal.insight.regression

import eu.seitzal.insight._

/**
 *  A simple version of the ordinary least squares (OLS) regression model.
 *  @param independent The data column containing the  training values for the independent variable
 *  @param dependent The data column containg the training values for the dependent variable 
 */
class SimpleOLS(independent : NumSeries, dependent : NumSeries) extends RegressionModel[Double, Double] {

  /** The slope of the regression line */
  val slope = (dependent cov independent) / independent.variance

  /** The Y intersect of the regression line */
  val intersect = dependent.avg - slope * independent.avg

  /** The pearson correlation coefficient of the two variables */
  lazy val r = independent pearson dependent

  /** The determination coefficient R² of the model */
  lazy val rsq = r * r

  /**
   *  Uses the model to estimate the value of the dependent variable for a new item.
   *  @param item The value for the dependent variable
   */
  def estimate(item : Double) : Double =
    slope * item + intersect

}

/** Companion object for the SimpleOLS class */
object SimpleOLS {

  /** Builds a new OLS model using data from the given dataset.
   *  @param data The dataset from which to build the model
   *  @param cname_independent The name of the independent variable (must be a numeric variable)
   *  @param cname_dependent The name of the dependent variable (must be a numeric variable)
   */
  def build(data : DataFrame, cname_independent : String, cname_dependent : String) =
    new SimpleOLS(data.num(cname_independent), data.num(cname_dependent))

}