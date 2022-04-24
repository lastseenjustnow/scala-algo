package design

import scala.collection.mutable

class UndergroundSystem() {

  type Time = Int
  type Id = Int
  type StationName = String
  val checksIn: mutable.Map[Id, (StationName, Time)] = mutable.Map[Id, (StationName, Time)]()
  val averages: mutable.Map[(StationName, StationName), (Time, Id)] = mutable.Map[(StationName, StationName), (Time, Int)]()

  def checkIn(id: Int, stationName: String, t: Int): Unit = {
    checksIn(id) = (stationName, t)
  }

  def checkOut(id: Int, stationName: String, t: Int): Unit = {
    val entranceT = checksIn(id)
    checksIn -= id
    val prevVal = averages.getOrElseUpdate((entranceT._1, stationName), (0, 0))
    averages.update((entranceT._1, stationName), (prevVal._1 + t - entranceT._2, prevVal._2 + 1))
  }

  def getAverageTime(startStation: String, endStation: String): Double = {
    val tup = averages((startStation, endStation))
    tup._1.toDouble / tup._2
  }

}
