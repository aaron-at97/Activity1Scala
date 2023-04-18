package cat.udl.eps.is

import munit.FunSuite
import scala.List

class StdLibSuite extends FunSuite {
  //sumFirstPowersOf
  test("sumFirstPowersOf") {
    assertEquals(StdLib.sumFirstPowersOf(2, 8), 255)
  }

  test("sumFirstPowersOf_value0") {  // valor (5, 1) 5 = b, 1 = n-1.
    assertEquals(StdLib.sumFirstPowersOf(5, 1), 1)
   }
   
  //countLengths
  test("countLengths") {
    val words = List("Hola", "Adios", "Scala")
    assertEquals(StdLib.countLengths(words), Map(4 -> 1, 5 -> 2))
  }

  test("countLengths_empty") {
    val words = List.empty[String]
    assertEquals(StdLib.countLengths(words), Map.empty[Int, Int])
  }

  test("countLengths_single") {
    val words = List("scala")
    assertEquals(StdLib.countLengths(words), Map(5 -> 1))
  }

}
