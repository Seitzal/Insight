package eu.seitzal.insight

import scala.collection.mutable.{ArrayBuffer, HashMap}

class ContingencyTable(rows : Series, cols : Series) {

  private val initResults = init()

  val rowLabels = initResults._1
  val colLabels = initResults._2
  val cells = initResults._3

  private def init() : (Array[String], Array[String], Array[Array[Int]]) = {
    val a_vals = new ArrayBuffer[String]()
    val b_vals = new ArrayBuffer[String]()
    val buffer = new HashMap[String, HashMap[String, Int]]()

    if (rows.length != cols.length)
      throw new ColLengthException("contingency table")

    for (i <- 0 until rows.length) {
      val ai = rows(i) match {
        case None => "NA"
        case x : Option[Double] @unchecked =>
          if (Helper.isInt(x))
            Helper.toIntString(x)
          else
            x.getOrElse(0.0).toString
        case any => any.toString
      }
      val bi = cols(i) match {
        case None => "NA"
        case x : Option[Double] @unchecked =>
          if (Helper.isInt(x))
            Helper.toIntString(x)
          else
            x.getOrElse(0.0).toString
        case any => any.toString
      }
      if (buffer.contains(ai)) {
        if (buffer(ai).contains(bi)) {
          buffer(ai)(bi) = buffer(ai)(bi) + 1
        } else {
          if (!b_vals.contains(bi))
            b_vals += bi
          buffer(ai)(bi) = 1
        }
      } else {
        a_vals += ai
        if (!b_vals.contains(bi))
          b_vals += bi
        buffer(ai) = new HashMap[String, Int]()
        buffer(ai)(bi) = 1
      }
    }

    val rls = a_vals.toArray
    val cls = b_vals.toArray

    val cells_ = for (rl <- rls)
      yield for (cl <- cls)
      yield if (buffer.contains(rl))
          buffer(rl).getOrElse(cl, 0)
        else 0

    (rls, cls, cells_)
  }

  def apply(rowLabel : String, colLabel : String) = {
    val row = rowLabels.indexOf(rowLabel)
    val col = colLabels.indexOf(colLabel)
    if (row == -1)
      throw new BadValueException(rowLabel)
    else if (col == -1)
      throw new BadValueException(colLabel)
    else
      cells(row)(col)
  }

  def apply(row : Int, col : Int) =
    cells(row)(col)

  def rel(rowLabel : String, colLabel : String) =
    apply(rowLabel, colLabel).toDouble / grandTotal

  def rel(row : Int, col : Int) =
    apply(row, col).toDouble / grandTotal

  lazy val rowTotal = {
    for (row <- 0 until rowLabels.length)
      yield cells(row).sum
  }.toArray

  lazy val colTotal = {
    for (col <- 0 until colLabels.length)
      yield cells.map(row => row(col)).sum
  }.toArray

  lazy val grandTotal = rowTotal.sum

  lazy val relCells =
    cells.map(row => row.map(cell => cell.toDouble / grandTotal))

  lazy val relRowTotal = rowTotal.map(rs => rs.toDouble / grandTotal)

  lazy val relColTotal = colTotal.map(cs => cs.toDouble / grandTotal)

  def rowProp(rowLabel : String, colLabel : String) = {
    val row = rowLabels.indexOf(rowLabel)
    if (row == -1) throw new BadValueException(rowLabel)
    apply(rowLabel, colLabel).toDouble / rowTotal(row)
  }

  def rowProp(row : Int, col : Int) =
    apply(row, col).toDouble / rowTotal(row)

  def colProp(rowLabel : String, colLabel : String) = {
    val col = colLabels.indexOf(colLabel)
    if (col == -1) throw new BadValueException(colLabel)
    apply(rowLabel, colLabel).toDouble / colTotal(col)
  }

  def colProp(row : Int, col : Int) =
    apply(row, col).toDouble / colTotal(col)

  lazy val rowProps = {
    for (row <- 0 until rowLabels.length)
      yield cells(row).map(cell => cell.toDouble / rowTotal(row))
  }.toArray

  lazy val colProps = {
    for (row <- 0 until rowLabels.length) yield {
      for (col <- 0 until colLabels.length) yield
        cells(row)(col).toDouble / colTotal(col)
    }.toArray
  }.toArray

  override def toString : String = {

    def longest(strs : Vector[String], current : Int = 0) : Int =
      if (strs.isEmpty)
        current
      else if (strs.head.length > current)
        longest(strs.tail, strs.head.length)
      else
        longest(strs.tail, current)

    def padding(str : String, length : Int) =
      (str.length until length).map(_ => " ").mkString

    def dashes(length : Int) =
      (0 until length).map(_ => "-").mkString

    val col1 = "" +: rowLabels.toVector :+ "Σ Col"
    val sumCol = "Σ Row" +: {
      for (row <- 0 until rowLabels.length)
        yield Vector(
          rowTotal(row).toString + "  ",
          relRowTotal(row).toString + " T",
          " ",
          " "
        )
    }.toVector.flatten
    val otherCols : List[Vector[String]] = {
      for (col <- 0 until colLabels.length) yield {
        (colLabels(col) + "  ") +: {
          for (row <- 0 until rowLabels.length) yield Vector(
            cells(row)(col).toString + "  ",
            Helper.roundTo(relCells(row)(col), 6).toString + " T",
            Helper.roundTo(rowProps(row)(col), 6).toString + " R",
            Helper.roundTo(colProps(row)(col), 6).toString + " C"
          )
        }.toVector.flatten
      } :+ (colTotal(col).toString + "  ") :+ (relColTotal(col).toString + " T")
    }.toList :+ sumCol
    val colWidths = otherCols.map(col => longest(col) + 1)
    val col1Width = longest(col1) + 1
    var buffer = ""
    def line() : Unit = {
      buffer = buffer + "+" + dashes(col1Width) + "-+"
      for (i <- 0 until colWidths.length)
        buffer = buffer + dashes(colWidths(i)) + "-+"
      buffer = buffer + "\n"
    }
    def emptyFirst() : Unit = {
      buffer = buffer + "|" + padding("", col1Width) + " |"
    }
    line()
    emptyFirst()
    for (i <- 0 until colWidths.length) {
      buffer = buffer +
        padding(otherCols(i).head, colWidths(i)) + otherCols(i).head + " |"
    }
    buffer = buffer + "\n"
    line()
    for (i <- 1 until col1.length - 1) {
      buffer = buffer + "|" + padding(col1(i), col1Width) + col1(i) + " |"
      val k = (i - 1) * 4 + 1
      for (l <- 0 to 3) {
        for (j <- 0 until otherCols.length) {
          buffer = buffer +
            padding(otherCols(j)(k + l), colWidths(j)) +
            otherCols(j)(k + l) +
            " |"
        }
        buffer = buffer + "\n"
        if (l < 3) emptyFirst()
      }
      line()
    }
    buffer = buffer + "|" + padding("Σ Col", col1Width) + "Σ Col |"
    for (col <- 0 until colLabels.length) {
      buffer = buffer +
        padding(colTotal(col).toString + "  ", colWidths(col)) +
        colTotal(col).toString + "  " +
        " |"
    }
    buffer = buffer + 
      padding(grandTotal.toString + "  ", colWidths(colLabels.length)) + 
      grandTotal.toString + 
      "   |\n"
    emptyFirst()
    for (col <- 0 until colLabels.length) {
      buffer = buffer +
        padding(relColTotal(col).toString + " T", colWidths(col)) +
        relColTotal(col).toString + " T" +
        " |"
    }
    buffer = buffer + padding("", colWidths(colLabels.length)) + " |\n"
    line()
    buffer
  }

}