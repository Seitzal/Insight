package eu.seitzal.insight

trait BorderedClass{
  def contains(x : Double) : Boolean
  def desc : String
}

class FloorBorderedClass(ceiling : Double) extends BorderedClass {
  def contains(x : Double) = (x < ceiling)
  def desc = "< " + ceiling.toString
}

class CeilingBorderedClass(floor : Double) extends BorderedClass {
  def contains(x : Double) = (x >= floor)
  def desc = ">= " + floor.toString
}

class DoubleBorderedClass(floor : Double, ceiling : Double)
    extends BorderedClass {
  def contains(x : Double) = (x < ceiling && x >= floor)
  def desc = ">= " + floor.toString + "; < " + ceiling.toString
}

object Aggregation {
  def aggregateByValue(data : DataFrame, cname : String, firstCeiling : Double, 
      width : Double, k : Int) = data.getCol(cname) match {

    case col : NumSeries => {
      // Generate classes
      val floor = new FloorBorderedClass(firstCeiling)
      val mid = for(i <- 0 until k - 2) 
                yield new DoubleBorderedClass(
                  firstCeiling + i * width, firstCeiling + (i + 1) * width)
      val ceil = new CeilingBorderedClass(firstCeiling + (k - 2) * width)
      val classes = floor +: mid.toVector :+ ceil

      //Count number of elements falling into each class
      def loop(remainder : List[Double], values : Vector[Int]) : Vector[Int] = {
        def check(x : Double, i : Int, values : Vector[Int]) : Vector[Int] =
          if(classes(i).contains(x))
            values.updated(i, values(i) + 1)
          else
          check(x, i + 1, values)
        if(remainder.isEmpty) values
        else loop(remainder.tail, check(remainder.head, 0, values))
      }
      val values_init = for(c <- classes)
                        yield 0
      val values = loop(col.existingValues.toList, values_init.toVector)

      //Generate new dataset
      val col_classes = new StrSeries(
        for(c <- classes)
        yield c.desc
      )
      val col_values = 
        new NumSeries(values.map((x : Int) => Option(x.toDouble)))
      new DataFrame(Map(
        cname -> col_classes,
        "n"   -> col_values
      ))
    }
    case StrSeries(col) => throw new NotNumericException(cname)
    case _           => throw new InvalidSeriesException(cname)
  }
}

