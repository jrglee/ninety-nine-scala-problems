import org.scalatest.{Matchers, FunSuite}

class ListSuite extends FunSuite with Matchers {

  test("P01 (*) Find the last element of a list.") {
    lists.last(List(1, 1, 2, 3, 5, 8)) shouldEqual 8
  }

  test("P02 (*) Find the last but one element of a list.") {
    lists.penultimate(List(1, 1, 2, 3, 5, 8)) shouldEqual 5
  }

  test("P03 (*) Find the Kth element of a list.") {
    lists.nth(2, List(1, 1, 2, 3, 5, 8)) shouldEqual 2
  }

  test("P04 (*) Find the number of elements of a list.") {
    lists.length(List(1, 1, 2, 3, 5, 8)) shouldEqual 6
  }

  test("P05 (*) Reverse a list.") {
    lists.reverse(List(1, 1, 2, 3, 5, 8)) shouldEqual List(8, 5, 3, 2, 1, 1)
  }

  test("P06 (*) Find out whether a list is a palindrome.") {
    lists.isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
  }

  test("P07 (**) Flatten a nested list structure.") {
    lists.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldEqual List(1, 1, 2, 3, 5, 8)
  }

  test("P08 (**) Eliminate consecutive duplicates of list elements.") {
    lists.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List('a, 'b, 'c, 'a, 'd, 'e)
  }

  test("P09 (**) Pack consecutive duplicates of list elements into sublists.") {
    lists.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }

  test("P10 (*) Run-length encoding of a list.") {
    lists.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }

  test("P11 (*) Modified run-length encoding.") {
    lists.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
  }

  test("P12 (**) Decode a run-length encoded list.") {
    lists.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) shouldEqual List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  test("P13 (**) Run-length encoding of a list (direct solution).") {
    lists.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }

  test("P14 (*) Duplicate the elements of a list.") {
    lists.duplicate(List('a, 'b, 'c, 'c, 'd)) shouldEqual List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }

  test("P15 (**) Duplicate the elements of a list a given number of times.") {
    lists.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) shouldEqual List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  test("P16 (**) Drop every Nth element from a list.") {
    lists.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  test("P17 (*) Split a list into two parts.") {
    lists.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual(List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  test("P18 (**) Extract a slice from a list.") {
    lists.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('d, 'e, 'f, 'g)
  }

  test("P19 (**) Rotate a list N places to the left.") {
    lists.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    lists.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }

  test("P20 (*) Remove the Kth element from a list.") {
    lists.removeAt(1, List('a, 'b, 'c, 'd)) shouldEqual(List('a, 'c, 'd), 'b)
  }

  test("P21 (*) Insert an element at a given position into a list.") {
    lists.insertAt('new, 1, List('a, 'b, 'c, 'd)) shouldEqual List('a, 'new, 'b, 'c, 'd)
  }

  test("P22 (*) Create a list containing all integers within a given range.") {
    lists.range(4, 9) shouldEqual List(4, 5, 6, 7, 8, 9)
  }

  test("P23 (**) Extract a given number of randomly selected elements from a list.") {
    val list = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    val selected = lists.randomSelect(3, list)

    selected should have length (3)
    selected.forall(list.contains) shouldBe true
    selected.distinct shouldEqual selected
  }

  test("P24 (*) Lotto: Draw N different random numbers from the set 1..M.") {
    val numbers = lists.lotto(6, 49)

    numbers should have length 6
    every(numbers) should be <= 49
  }

  ignore("P25 (*) Generate a random permutation of the elements of a list.") {
    lists.randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)) shouldEqual List('b, 'a, 'd, 'c, 'e, 'f)
  }

  ignore("P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.") {
    lists.combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) should contain(List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e)))
  }
}