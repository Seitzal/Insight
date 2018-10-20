package eu.seitzal.insight

sealed trait SortMode
object SortMode {
  object ASCENDING extends SortMode
  object DESCENDING extends SortMode
}