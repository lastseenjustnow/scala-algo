import scala.math.abs

object General {
  def assignBikes(workers: Array[Array[Int]], bikes: Array[Array[Int]]): Array[Int] = {

    val workersIds = workers.zipWithIndex
    val bikesIds = bikes.zipWithIndex
    val distancesSorted = workersIds
      .flatMap(
        worker => bikesIds
          .map(bike => (abs(worker._1(0) - bike._1(0)) + abs(worker._1(1) - bike._1(1)), worker._2, bike._2))
      ).sorted.map(x => (x._2, x._3))

    val bikesTaken = Array.fill(bikes.length)(false)
    val workersAssigned = Array.fill(workers.length)(-1)
    var workersReceivedBike = 0
    var i = 0

    while (workersReceivedBike != workers.length) {
      val currentRule = distancesSorted(i)
      if (workersAssigned(currentRule._1) == -1 & !bikesTaken(currentRule._2)) {
        workersAssigned(currentRule._1) = currentRule._2
        bikesTaken(currentRule._2) = true
        workersReceivedBike += 1
      }
      i += 1
    }
    workersAssigned
  }
}
