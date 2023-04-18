package cat.udl.eps.is

import munit.FunSuite
import List.*

class ListSuite extends FunSuite {

  //partition
  test("partition_empty") {
    assertEquals(partition(Nil)(_ => true), (Nil, Nil))
  }

  test("partition_true") {
    assertEquals(partition(List(1, 2, 3))(_ => true), (List(1, 2, 3), Nil))
  }

  test("partition_false") {
    assertEquals(partition(List(1, 2, 3))(_ => false), (Nil, List(1, 2, 3)))
  }

  //partitionMap (via foldRight)
  test("partitionMap_empty") {
    assertEquals(partitionMap(Nil)(_ => Left(0)), (Nil, Nil))
  }

  test("partitionMap_Left") {
    assertEquals(partitionMap(List(1, 2, 3))((a: Int) => Left(a * 3)), (List(3, 6, 9), Nil))
  }

  test("partitionMap_Right") {
    assertEquals(partitionMap(List(1, 2, 3))((a: Int) => Right(a * 3)), (Nil, List(3, 6, 9)))
  }

  //find
  test("find_empty") {
    val l = Nil
    assertEquals(find(l)(_ => true), None)
  }

  test("find_true") {
    val l = Cons(1, Cons(2, Cons(3, Nil)))
    assertEquals(find(l)(_ == 2), Some(2))
  }

  test("find_false") {
    val l = Cons(1, Cons(2, Cons(3, Nil)))
    assertEquals(find(l)(_ == 4), None)
  }

  //sort1
  test("sort1") {
    val l = Cons(3, Cons(1, Cons(2, Nil)))
    val result = Cons(3, Cons(2, Cons(1, Nil)))
    assertEquals(sort1(l)(_ >= _), result)
  }
  test("sort1_sorted") {
  val l = Cons(1, Cons(2, Cons(3, Nil)))
  assertEquals(sort1(l)(_ <= _), l)
  }

  test("sort1_reverse") {
    val l = Cons(3, Cons(2, Cons(1, Nil)))
    val expected = Cons(1, Cons(2, Cons(3, Nil)))
    assertEquals(sort1(l)(_ <= _), expected)
  }

  test("sort1_singleton") {
    val l = Cons(1, Nil)
    assertEquals(sort1(l)(_ <= _), l)
  }

  //digitsToNum
  test("digitsToNum_empty") {
    assertEquals(digitsToNum(Nil), 0)
  }

  test("digitsToNum_singleton") {
    assertEquals(digitsToNum(List(3)), 3)
  }

  test("digitsToNum") {
    assertEquals(digitsToNum(List(1, 2, 3)), 123)
  }

  //mergeSorted

  test("mergeSorted_oneEmptyList") {
    assertEquals(mergeSorted(List(1, 2, 3), Nil)(_ <= _), List(1, 2, 3))
    assertEquals(mergeSorted(Nil, List(1, 2, 3))(_ <= _), List(1, 2, 3))
  }

  test("mergeSorted") {
    assertEquals(mergeSorted(List(10, 20, 30), List(15, 25, 35))(_ <= _), List(10, 15, 20, 25, 30, 35))
    assertEquals(mergeSorted(List(8, 7, 2), List(5, 4, 1))(_ >= _), List(8, 7, 5, 4, 2, 1))
  }

  //digitsToNumEither
  test("digitsToNumEither_valid") {
    assertEquals(digitsToNumEither(List(1, 2, 3)), Right(123))
  }

  test("digitsToNumEither_invalid") {
    assertEquals(digitsToNumEither(List(1, 2, 11)), Left(" 11 dígito no válido"))
  }

  test("digitsToNumEither_empty") {
    assertEquals(digitsToNumEither(Nil), Right(0))
  }
  
}
