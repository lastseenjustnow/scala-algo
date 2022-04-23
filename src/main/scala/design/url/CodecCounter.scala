package design.url

import scala.collection.mutable

class CodecCounter {
  val map: mutable.Map[Int, String] = mutable.Map[Int, String]()
  var i = 0

  def encode(longURL: String): String = {
    map.put(i, longURL)
    i += 1
    "http://tinyurl.com/" + (i - 1)
  }

  // Decodes a shortened URL to its original URL.
  def decode(shortURL: String): String = {
    map(shortURL.replace("http://tinyurl.com/", "").toInt)
  }
}