package cat.udl.eps.is

import munit.FunSuite
import scala.List
import BST.*

class BSTSuite extends FunSuite {

  // find
  test("find_empty") {
    val t = Empty
    assertEquals(t.find(1)(_ < _), false)
  }

  test("find_singleton") {
    val t = Node(Empty, 1, Empty)
    assertEquals(t.find(1)(_ < _), true)
  }

  test("find_multiple") {
    val t = Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty))
    assertEquals(t.find(3)(_ < _), true)
  }

  test("find_notMultiple") {
    val t = Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty))
    assertEquals(t.find(8)(_ < _), false)
  }

  // Insert
  test("insert_Empty") {
    val l = Empty
    val t = l.insert(1)(_ < _)
    assertEquals(t, Node(Empty, 1, Empty))
  }

  test("insert_small") {
    val l = Node(Empty, 2, Empty)
    val t = l.insert(1)(_ < _)
    assertEquals(t, Node(Node(Empty, 1, Empty), 2, Empty))
  }

  test("insert_large") {
    val l = Node(Empty, 2, Empty)
    val t = l.insert(3)(_ < _)
    assertEquals(t, Node(Empty, 2, Node(Empty, 3, Empty)))
  }

  test("insert_equal") {
    val l = Node(Empty, 2, Empty)
    val t = l.insert(2)(_ < _)
    assertEquals(t, Node(Empty, 2, Empty))
  }



  //fold
  test("fold_singleton") {
    val t = Node(Empty, 5, Empty).value
    assertEquals(t, 5)
  }

  test("fold_desendent") {
    val t = Node(Node(Node(Empty, 5, Empty), 4, Node(Empty, 3, Empty)), 2, Node(Node(Empty, 1, Empty), 6, Node(Empty, 7, Empty)))
    assertEquals(t.fold("")((acc, value, elem) => acc + value + elem ), "5432167")
  }

  test("fold_order") {
    val t = Node(Node(Empty, 1, Node(Empty, 2, Empty)), 3, Node(Node(Empty, 4, Empty), 5, Node(Empty, 6, Empty)))
    assertEquals(t.fold("")((acc, value, elem) => acc + value + elem ), "123456")
  }

  
  //fromList
  test("fromList_empty") {
    val l: List[Int] = List.empty
    val t = fromList(l)(_ < _)
    assertEquals(t, Empty)
  }

  test("fromList_singleton") {
    val l: List[Int] = List(1)
    val t = fromList(l)(_ < _)
    assertEquals(t, Node(Empty, 1, Empty))
  }

  test("fromList_unsorted") {
    val l = List(3, 2, 1, 4)
    val t = fromList(l)(_ < _)
    val result = Node(Node(Node(Empty,1,Empty),2,Empty),3,Node(Empty,4,Empty))
    assertEquals(t, result)
  }

  test("fromList_ascendente") {
    val l = List(1, 2, 3, 4, 5)
    val t = fromList_2(l)(_ < _)
    assertEquals(t, Node(Node(Node(Node(Node(Empty, 1, Empty), 2, Empty), 3, Empty), 4, Empty), 5, Empty))
  }

  test("fromList_descendente") {
    val l = List(5, 4, 3, 2, 1)
    val t = fromList_2(l)(_ > _)
    assertEquals(t, Node(Node(Node(Node(Node(Empty, 5, Empty), 4, Empty), 3, Empty), 2, Empty), 1, Empty))
  }


  //inorder
  test("roundtrip") {
    val l = List(3, 2, 1, 4)
    val t = fromList(l)(_ < _)
    assertEquals(inorder(t), l.sorted)
  }

  test("inorder_empty") {
    val t = Empty
    assertEquals(inorder(t), List.empty)
  }

  test("inorde_singleton") {
    val t = Node(Empty, 1, Empty)
    assertEquals(inorder(t), List(1))
  }

  test("inorder_listOrder") {
    val t = Node(Node(Node(Empty, 1, Empty), 2, Empty), 3, Empty)
    assertEquals(inorder(t), List(1, 2, 3))
  }

}
