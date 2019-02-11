package eu.seitzal.insight.read

import eu.seitzal.insight._

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec

class CSVReader(path : String) {

  def buildFrame : DataFrame = {
    val src = Source.fromFile(path).getLines
    val raw = parse(src, Vector())
    val cnames = raw.map(col => col.head)
    val cols = raw.map(col => {
      val asStr = StrSeries(col.tail.par)
      Try(asStr.toNumSeries) match {
        case Success(numS) => numS
        case Failure(e) => asStr
      }
    })
    new DataFrame(cnames zip cols)
  }

  @tailrec final def parse(src : Iterator[String], 
      buffer : Vector[Vector[String]]) : Vector[Vector[String]] =
    if (!src.hasNext)
      buffer
    else
      parse(src, parseLine(src.next, buffer, "", 0, 0, false))
  
  @tailrec final def parseLine(line : String,buffer : Vector[Vector[String]], 
      cellBuffer : String, pos : Int, col : Int, escaped : Boolean
      ) : Vector[Vector[String]] =
    if (pos == line.length)
      if (buffer.length <= col)
        buffer :+ Vector(cellBuffer)
      else
        buffer.updated(col, buffer(col) :+ cellBuffer)
    else if (line.charAt(pos) == '\"')
      parseLine(line, buffer, cellBuffer, pos + 1, col, !escaped)
    else if (escaped || line.charAt(pos) != ',')
      parseLine(line, buffer, cellBuffer + line.charAt(pos), pos + 1, col, 
        escaped)
    else if (buffer.length <= col)
      parseLine(line, buffer :+ Vector(cellBuffer), "", pos + 1, col + 1, false)
    else
      parseLine(line, buffer.updated(col, buffer(col) :+ cellBuffer), "", 
        pos + 1, col + 1, false)

} 