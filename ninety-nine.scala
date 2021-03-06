import util.Random

// P01 Find the last element of a list.
def last(list: List[Int]) = list.last

// P02 Find the last but one element of a list.
def penultimate(list: List[Int]) = list.takeRight(2).head

// P03  Find the Kth element of a list.
def nth(n: Int, list: List[Int]) = list(n)

// P04 Find the number of elements of a list.
def length(list: List[Any]) = list.length

// P05 Reverse a list.
def reverse(list: List[Any]): List[Any] = list.reverse

// P06 Find out whether a list is a palindrome.
def isPalindrome(list: List[Any]) = list == reverse(list)

// P07 Flatten a nested list structure.
def flatten(list: List[Any]): List[Any] = list match {
  case (head: List[Any]) :: tail => flatten(head) ::: flatten(tail)
  case head :: tail => head :: flatten(tail)
  case Nil => Nil
}

// P08 Eliminate consecutive duplicates of list elements.
def compress[T](list: List[T]): List[T] = list match {
  case head :: next :: tail =>
    if (head == next)
      compress(next :: tail)
    else
      head :: compress(next :: tail)
  case head :: Nil => List(head)
  case Nil => Nil
}

// P09 Pack consecutive duplicates of list elements into sublists.
def pack[T](list: List[T]): List[List[T]] = list match {
  case head :: tail =>
    val count: Int = 1 + (tail takeWhile (_ == head) length)
    List.fill(count)(head) :: pack(tail.drop(count - 1))
  case Nil => Nil
}

// P10 Run-length encoding of a list.
def encode[T](list: List[T]): List[(Int, T)] = pack(list) map (l => (l.length, l.head))

// P11 Modified run-length encoding.
def encodeModified[T](list: List[T]): List[Any] = {
  encode(list) map {
    case (1, value) => value
    case (count, value) => (count, value)
  }
}

// P12 Decode a run-length encoded list.
def decode[T](list: List[(Int, T)]): List[T] = list match {
  case (c, v) :: tail => List.fill(c)(v) ::: decode(tail)
  case Nil => Nil
}

// P13 Run-length encoding of a list (direct solution).
def encodeDirect[T](list: List[T]): List[(Int, T)] = list match {
  case head :: tail =>
    val count: Int = 1 + (tail takeWhile (_ == head) length)
    (count, head) :: encodeDirect(tail.drop(count))
  case Nil => Nil
}

// P14 Duplicate the elements of a list.
def duplicate[T](list: List[T]): List[T] = list match {
  case head :: tail => List.fill(2)(head) ::: duplicate(tail)
  case Nil => Nil
}

// P15 Duplicate the elements of a list a given number of times.
def duplicateN[T](times: Int, list: List[T]): List[T] = list match {
  case head :: tail => List.fill(times)(head) ::: duplicateN(times, tail)
  case Nil => Nil
}

// P16 Drop every Nth element from a list.
def drop[T](n: Int, list: List[T]): List[T] = list match {
  case l if l.length >= n => l.take(n - 1) ::: drop(n, l.drop(n))
  case tail => tail
}

// P17 Split a list into two parts.
def split[T](n: Int, list: List[T]): (List[T], List[T]) = list match {
  case l if l.length >= n => (l.take(n), l.drop(n))
  case tail => (tail, List())
}

// P18 Extract a slice from a list.
def slice[T](from: Int, to: Int, list: List[T]): List[T] = list match {
  case l if l.length >= to => l.slice(from, to)
  case l if l.length >= from => l.slice(from, l.length)
  case l if l.length <= from => List()
  case l => l
}

// P19 Rotate a list N places to the left.
def rotate[T](times: Int, list: List[T]): List[T] = times match {
  case n if n > 0 => rotate(n - 1, list.tail :+ list.head)
  case n if n < 0 => rotate(n + 1, list.last :: list.init)
  case n => list
}

// P20 Remove the Kth element from a list.
def removeAt[T](index: Int, list: List[T]): (List[T], T) = {
  val (start, end) = list.splitAt(index)
  (start ::: end.tail, list(index))
}

// P21 Insert an element at a given position into a list.
def insertAt[T](value: T, index: Int, list: List[T]): List[T] = {
  val (start, end) = list.splitAt(index)
  start ::: List(value) ::: end
}

// P22 Create a list containing all integers within a given range.
def range(start: Int, end: Int): List[Int] = start to end toList

// P23 Extract a given number of randomly selected elements from a list.
def randomSelect[T](n: Int, list: List[T]): List[T] = n match {
  case n if n > 0 =>
    val (sublist, e) = removeAt(Random.nextInt(list.length), list)
    e :: randomSelect(n - 1, sublist)
  case 0 => List()
}

// P24 Lotto: Draw N different random numbers from the set 1..M.
def lotto(n: Int, max: Int): List[Int] = randomSelect(n, range(1, max))

// P25 Generate a random permutation of the elements of a list.
def randomPermute[T](list: List[T]): List[T] = randomSelect(list.length, list)

// P26 Generate the combinations of K distinct objects chosen from the N elements of a list.
// Note: the easy solution is list.combinations(size).toList
def combinations[T](size: Int, list: List[T]): List[List[T]] = size match {
  case n if n > 1 =>
    var result: List[List[T]] = List()
    for (i <- 0 to list.length - size) {
      combinations(n - 1, list.drop(i + 1)) foreach (result :+= list(i) :: _)
    }
    result
  case 1 => list map (List(_))
}

// P27 Group the elements of a set into disjoint subsets.

// a)
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

// b)
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

// P28 Sorting a list of lists according to length of sublists.
// a)
def lsort[T](list: List[List[T]]): List[List[T]] = list sortBy (x => x.length)

// b)
def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
  val frequencyMap = lsort(list) groupBy (_.length)
  var result: List[List[T]] = List()
  frequencyMap.keySet.toList.sorted foreach (freqs => result :::= (list filter (_.length == freqs)))
  result
}