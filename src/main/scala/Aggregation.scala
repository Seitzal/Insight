package seitzal.scalastat

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

class DoubleBorderedClass(floor : Double, ceiling : Double) extends BorderedClass {
  def contains(x : Double) = (x < ceiling && x >= floor)
  def desc = ">= " + floor.toString + "; < " + ceiling.toString
}

