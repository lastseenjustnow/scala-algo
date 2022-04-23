package design

class ATM() {

  val availBanknotes: Array[Long] = Array(0, 0, 0, 0, 0).map(_.toLong)
  val banknotesMap: Array[Int] = Array(20, 50, 100, 200, 500)

  def deposit(banknotesCount: Array[Int]): Unit = {
    banknotesCount.zipWithIndex.foreach(x => availBanknotes(x._2) += x._1)
  }

  def withdraw(amount: Int): Array[Int] = {
    var lastIndex = availBanknotes.zip(banknotesMap).lastIndexWhere(x => x._2 <= amount && x._1 > 0)
    var currentAmount: Long = amount
    val subtractBanknotes = Array.fill(5)(0L)
    while (lastIndex >= 0) {
      subtractBanknotes(lastIndex) = (currentAmount / banknotesMap(lastIndex)) min availBanknotes(lastIndex)
      currentAmount -= subtractBanknotes(lastIndex) * banknotesMap(lastIndex)
      lastIndex -= 1
    }
    val bankNotesDiff = availBanknotes.zip(subtractBanknotes).map(x => x._1 - x._2)

    if (currentAmount > 0 || bankNotesDiff.exists(x => x < 0)) Array(-1) else {
      for (i <- subtractBanknotes.indices) availBanknotes(i) -= subtractBanknotes(i)
      subtractBanknotes.map(_.toInt)
    }
  }

}