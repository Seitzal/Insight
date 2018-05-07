package eu.seitzal.scalastat

sealed trait SortMode
object SortMode {
  object ASCENDING extends SortMode
  object DESCENDING extends SortMode
}