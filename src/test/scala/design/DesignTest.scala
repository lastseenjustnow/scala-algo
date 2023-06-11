package design

import org.scalatest.FunSuite

class DesignTest extends FunSuite {
  test("design.ATM Design") {
    val atm1 = new ATM()
    atm1.deposit(Array(0, 10, 0, 3, 0))
    assert(atm1.withdraw(500) sameElements Array(0, 2, 0, 2, 0))

    val atm2 = new ATM()
    atm2.deposit(Array(0, 0, 1, 2, 1))
    assert(atm2.withdraw(600) sameElements Array(0, 0, 1, 0, 1))
    atm2.deposit(Array(0, 1, 0, 1, 1))
    assert(atm2.withdraw(600) sameElements Array(-1))
    assert(atm2.withdraw(550) sameElements Array(0, 1, 0, 0, 1))

  }

  test("Underground System") {
    val metro = new UndergroundSystem()
    metro.checkIn(45, "Leyton", 3)
    metro.checkIn(32, "Paradise", 8)
    metro.checkIn(27, "Leyton", 10)
    metro.checkOut(45,"Waterloo",15)
    metro.checkOut(27,"Waterloo",20)
    metro.checkOut(32,"Cambridge",22)
    assert(metro.getAverageTime("Paradise","Cambridge") == 14)
    assert(metro.getAverageTime("Leyton","Waterloo") == 11)
    metro.checkIn(10,"Leyton",24)
    assert(metro.getAverageTime("Leyton","Waterloo") == 11)
    metro.checkOut(10,"Waterloo",38)
    assert(metro.getAverageTime("Leyton","Waterloo") == 12)
  }

  test("Snapshot Array") {
    val instance = new SnapshotArray(4)
    instance.set(0, 5)
    instance.snap()
    instance.set(0, 6)
    assert(instance.get(0, 0) == 5)
    instance.set(0, 12)
    assert(instance.get(0, 1) == 12)
    assert(instance.get(0, 15) == 12)

    val instance2 = new SnapshotArray(4)
    instance2.snap()
    instance2.snap()
    assert(instance2.get(3, 1) == 0)
    instance2.set(2, 4)
    instance2.snap()
    instance2.set(1, 4)
    assert(instance2.get(3, 2) == 0)
    assert(instance2.get(1, 2) == 0)
    assert(instance2.get(1, 3) == 4)

    val instance3 = new SnapshotArray(2)
    instance3.snap()
    assert(instance3.get(1, 0) == 0)
    assert(instance3.get(0, 0) == 0)
    instance3.set(1, 8)
    assert(instance3.get(1, 0) == 0)
    instance3.set(0, 20)
    assert(instance3.get(0, 0) == 0)
    instance3.set(0, 7)
    assert(instance3.get(0, 0) == 0)
    assert(instance3.get(0, 1) == 7)
  }

}
