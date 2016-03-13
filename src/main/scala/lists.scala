import scala.util.Random

package object lists {

  def last(list: List[Int]) = list.last

  def penultimate(list: List[Int]) = list.takeRight(2).head

  def nth(n: Int, list: List[Int]) = list(n)

  def length(list: List[Any]) = list.length

  def reverse(list: List[Any]): List[Any] = list.reverse

  def isPalindrome(list: List[Any]) = list == reverse(list)

  def flatten(list: List[Any]): List[Any] = list match {
    case (head: List[Any]) :: tail => flatten(head) ::: flatten(tail)
    case head :: tail => head :: flatten(tail)
    case Nil => Nil
  }

  def compress[T](list: List[T]): List[T] = list match {
    case head :: next :: tail =>
      if (head == next)
        compress(next :: tail)
      else
        head :: compress(next :: tail)
    case head :: Nil => List(head)
    case Nil => Nil
  }

  def pack[T](list: List[T]): List[List[T]] = list match {
    case head :: tail =>
      val count: Int = 1 + (tail takeWhile (_ == head) length)
      List.fill(count)(head) :: pack(tail.drop(count - 1))
    case Nil => Nil
  }

  def encode[T](list: List[T]): List[(Int, T)] = pack(list) map (l => (l.length, l.head))

  def encodeModified[T](list: List[T]): List[Any] = {
    encode(list) map {
      case (1, value) => value
      case (count, value) => (count, value)
    }
  }

  def decode[T](list: List[(Int, T)]): List[T] = list match {
    case (c, v) :: tail => List.fill(c)(v) ::: decode(tail)
    case Nil => Nil
  }

  def encodeDirect[T](list: List[T]): List[(Int, T)] = list match {
    case head :: tail =>
      val count: Int = 1 + (tail takeWhile (_ == head) length)
      (count, head) :: encodeDirect(tail.drop(count - 1))
    case Nil => Nil
  }

  def duplicate[T](list: List[T]): List[T] = list match {
    case head :: tail => List.fill(2)(head) ::: duplicate(tail)
    case Nil => Nil
  }

  def duplicateN[T](times: Int, list: List[T]): List[T] = list match {
    case head :: tail => List.fill(times)(head) ::: duplicateN(times, tail)
    case Nil => Nil
  }

  def drop[T](n: Int, list: List[T]): List[T] = list match {
    case l if l.length >= n => l.take(n - 1) ::: drop(n, l.drop(n))
    case tail => tail
  }

  def split[T](n: Int, list: List[T]): (List[T], List[T]) = list match {
    case l if l.length >= n => (l.take(n), l.drop(n))
    case tail => (tail, List())
  }

  def slice[T](from: Int, to: Int, list: List[T]): List[T] = list match {
    case l if l.length >= to => l.slice(from, to)
    case l if l.length >= from => l.slice(from, l.length)
    case l if l.length <= from => List()
    case l => l
  }

  def rotate[T](times: Int, list: List[T]): List[T] = times match {
    case n if n > 0 => rotate(n - 1, list.tail :+ list.head)
    case n if n < 0 => rotate(n + 1, list.last :: list.init)
    case n => list
  }

  def removeAt[T](index: Int, list: List[T]): (List[T], T) = {
    val (start, end) = list.splitAt(index)
    (start ::: end.tail, list(index))
  }

  def insertAt[T](value: T, index: Int, list: List[T]): List[T] = {
    val (start, end) = list.splitAt(index)
    start ::: List(value) ::: end
  }

  def range(start: Int, end: Int): List[Int] = start to end toList

  def randomSelect[T](n: Int, list: List[T]): List[T] = n match {
    case n if n > 0 =>
      val (sublist, e) = removeAt(Random.nextInt(list.length), list)
      e :: randomSelect(n - 1, sublist)
    case 0 => List()
  }

  def lotto(n: Int, max: Int): List[Int] = randomSelect(n, range(1, max))

  def randomPermute[T](list: List[T]): List[T] = randomSelect(list.length, list)

  def combinations[T](size: Int, list: List[T]): List[List[T]] = size match {
    case n if n > 1 =>
      var result: List[List[T]] = List()
      for (i <- 0 to list.length - size) {
        combinations(n - 1, list.drop(i + 1)) foreach (result :+= list(i) :: _)
      }
      result
    case 1 => list map (List(_))
  }


  def group3[T](list: List[T]): List[List[List[T]]] = {
    var result: List[List[List[T]]] = List()
    combinations(2, list) foreach {
      g2 =>
        combinations(3, (list diff g2)) foreach {
          g3 =>
            combinations(4, list diff (g2 ::: g3)) foreach {
              g4 =>
                result :+= List(g2, g3, g4)
            }
        }
    }
    result
  }

  def group[T](pattern: List[Int], list: List[T]): List[List[List[T]]] = pattern match {
    case head :: Nil => combinations(head, list) map (l => List(l))
    case head :: tail =>
      var result: List[List[List[T]]] = List()
      combinations(head, list) foreach {
        g =>
          group(tail, (list diff g)) foreach (l => result :+= g :: l)
      }
      result
    case Nil => Nil
  }

  def lsort[T](list: List[List[T]]): List[List[T]] = list sortBy (x => x.length)

  def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
    val frequencyMap = lsort(list) groupBy (_.length)
    var result: List[List[T]] = List()
    frequencyMap.keySet.toList.sorted foreach (freqs => result :::= (list filter (_.length == freqs)))
    result
  }
}