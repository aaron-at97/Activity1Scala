package cat.udl.eps.is

import scala.List

enum BST[+A]:
  case Empty
  case Node(left: BST[A], value: A, right: BST[A])


  // 1. insert

  // La funció que passem ens indica si el primer paràmetre és més petit (<) que el segon
  def insert[B >: A](a: B)(lt: (B, B) => Boolean): BST[B] = this match
    case Empty => Node(Empty, a, Empty)
    case Node(left, value, right) =>
      if (lt(a, value)) Node(left.insert(a)(lt), value, right)
      else if (lt(value, a)) Node(left, value, right.insert(a)(lt))
      else this // el valor ya existe en el árbol

  // 2.find

  def find[B >: A](a: B)(lt: (B, B) => Boolean): Boolean = this match
    case Empty => false
    case Node(left, value, right) =>
      if (a == value) true
      else if (lt(a, value)) left.find(a)(lt)
      else right.find(a)(lt)

  // 3.fold

  // També és un dels que no ho provocat majors problemes
  def fold[B](b: B)(f: (B, A, B) => B): B = this match
      case Empty => b
      case Node(left, value, right) =>
        f(left.fold(b)(f), value, right.fold(b)(f))

object BST:

  // Les llistes que farem servir són les de scala
  
  // 4. fromList
  def fromList[A](l: List[A])(lt: (A, A) => Boolean): BST[A] = 
    l.foldLeft(Empty: BST[A])((acc, elem) => acc.insert(elem)(lt))

  // 4.1 fromList (via FoldRight) Ampliació Aaron Arenas
  def fromList_2[A](l: List[A])(lt: (A, A) => Boolean): BST[A] =
    l.foldRight(Empty: BST[A])((elem, acc) => acc.insert(elem)(lt))

  // 5. inorder (via fold)
  def inorder[A](t: BST[A]): List[A] = 
    t.fold(List[A]())((acc, value, right) => acc ++ (value :: right))
