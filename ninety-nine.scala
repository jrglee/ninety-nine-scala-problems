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
def compress(list: List[Symbol]): List[Symbol] = list match {
  case head :: next :: tail =>
    if (head == next)
      compress(next :: tail)
    else
      head :: compress(next :: tail)
  case head :: Nil => List(head)
  case Nil => Nil
}

// P09 Pack consecutive duplicates of list elements into sublists.
def pack(list: List[Any]): List[List[Any]] = list match {
  case (head: List[Any]) :: next :: tail =>
    if (head(0) == next)
      pack((head :+ next) :: tail)
    else
      head :: pack(next :: tail)
  case (head: List[Any]) :: Nil => List(head)
  case head :: next :: tail =>
    if (head == next)
      pack(List(head, next) :: tail)
    else
      List(head) :: pack(next :: tail)
  case head :: Nil => List(List(head))
  case Nil => Nil
}


// P10 Run-length encoding of a list.
def encode(list: List[Any]): List[(Int, Any)] = pack(list) map (symbols => (symbols.length, symbols(0)))