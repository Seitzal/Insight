package eu.seitzal.insight.regression

import eu.seitzal.insight._

/**
 *  Abstract representation of a regression model.
 *  @tparam T The data type of the independent variable(s)
 *  @tparam U The data type of the dependent variable
 */
trait RegressionModel[T, U] {

  /**
   *  Uses the model to estimate the value of the dependent variable for a new item.
   *  @param item The value(s) for the dependent variable(s)
   */
  def estimate(item : T) : U
} 