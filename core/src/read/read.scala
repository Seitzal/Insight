package eu.seitzal.insight

package object read {

  /**
   * Attempts to read a data frame from a CSV file at the specified location.
   * The first line of the CSV is assumed to contain the variable names.
   * @param path The path to the CSV file.
   * @return A dataset object containing the data from the file.
   */
  def csv(path : String) : DataFrame =
    new CSVReader(path).buildFrame

}