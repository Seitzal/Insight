package eu.seitzal.insight

import java.io.IOException

package object write {

  @throws(classOf[IOException])
  def html(data : DataFrame, path : String) : Unit = {
    val builder = HtmlBuilder.fromDataFrame(data)
    builder.export(path)
  }

  @throws(classOf[IOException])
  def html(contingencyTable : ContingencyTable, path : String) : Unit = {
    val builder = HtmlBuilder.fromContingencyTable(contingencyTable)
    builder.export(path)
  }

  def csv(data : DataFrame, path : String) : Unit = {
    val writer = new CSVWriter(path)
    writer.writeDataFrame(data)
  }

}