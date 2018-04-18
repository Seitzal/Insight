package seitzal.scalastat

object Helper {

  def isInt(x : Double) = (x.toInt - x == 0.0)

  def round(x : Double) : Double = math.round(x * 10000000d).asInstanceOf[Double] / 10000000d
  def roundTo(x : Double, decimals : Int) = {
    val factor = math.pow(10, decimals).toDouble
    math.round(x * factor).asInstanceOf[Double] / factor
  }

  def nodoubles(l : List[String], acc : List[String] = Nil) : Boolean = {
    if(l.isEmpty) true
    else if(acc.contains(l.head)) false
    else nodoubles(l.tail, l.head :: acc)
  }

}