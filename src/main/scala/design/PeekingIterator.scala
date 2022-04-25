package design

class PeekingIterator(_iterator: Iterator[Int]) {

  var peekValue: Option[Int] = Some(_iterator.next())

  def peek(): Int = peekValue.get

  def next(): Int = {
    val oldPeekValue = peekValue.get
    peekValue = if (_iterator.hasNext) Some(_iterator.next()) else None
    oldPeekValue
  }

  def hasNext: Boolean = {
    if (peekValue.isEmpty) false else true
  }
}
