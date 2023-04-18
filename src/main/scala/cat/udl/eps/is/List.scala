package cat.udl.eps.is

// Exercise in user defines lists

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:

  def apply[A](as: A*): List[A] =
    // This foldRight is the predefined one on Seq
    as.foldRight(Nil: List[A])(Cons.apply)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], acc: B)(f: (B, A) => B): B =
    l match
      case Nil         => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x))(f)

  // If we put a function in a single parameter list we can define the function
  // with {} instead of ()
  def reverse[A](l: List[A]): List[A] =
    foldLeft[A, List[A]](l, Nil) { (acc, x) =>
      Cons(x, acc)
    }

  def foldRight[A, B](l: List[A], acc: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), acc) { (acc, x) =>
      f(x, acc)
    }

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons.apply)

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  // ------------------------------------------------------------------

  // 1. partition (via foldRight)

  // Separa en dues llistes els que cumpleixen el predicats dels que no ho fan
  // - la primera llista conté els que el compleixen, la segona els que no
  // - l'ordre dels elements es manté, és a dir:
  //     partition(l, _ => true) == (l, Nil)
  //     partition(l, _ => false) == (Nil, l)

  def partition[A](l: List[A])(p: A => Boolean): (List[A], List[A]) = 
    foldRight(l, (List[A](), List[A]())) { (a, acc) =>
        val (t, f) = acc
        if p(a) then (Cons(a, t), f) else (t, Cons(a, f))
    }

  // 2. partitionMap (via foldRight)

  // Semblant a l'anterior però fent servir un Either per decidir si a la llista
  // de l'esquerra o de la dreta

  def partitionMap[A, B, C](l: List[A])(p: A => Either[B, C]): (List[B], List[C]) = 
    foldRight(l, (List[B](), List[C]())) { (a, acc) =>
      val (t, f) = acc
      p(a) match
        case Left(b)  => (Cons(b, t), f)
        case Right(c) => (t, Cons(c, f))
    }

  // 3. find (stack-safe)

  // Busca el primer element que compleix una condició

  @annotation.tailrec
  def find[A](l: List[A])(p: A => Boolean): Option[A] =
    l match
      case Nil         => None
      case Cons(a, as) => if p(a) then Some(a) else find(as)(p)

  // 4. sort (insertion sort via foldLeft)

  // La funció que passem és indica si el primer paràmetre és més petit o igual (<=) que el segon
  // PISTA: feu servir una funció auxiliar per a inserir de forma ordenada

  def sort1[A](as: List[A])(lte: (A, A) => Boolean): List[A] =
    def insert(a: A, l: List[A]): List[A] = l match
      case Nil => Cons(a, Nil)
      case Cons(b, bs) =>
        if (lte(a, b)) Cons(a, l)
        else Cons(b, insert(a, bs))
    foldLeft(as, List[A]())((acc, a) => insert(a, acc))

  // 5. digitsToNum (use a fold, choose wisely)

  // Podeu suposar que la llista està realment formada per dígits
  // List(1, 2, 3) -> 123

  def digitsToNum(l: List[Int]): Int =
    foldLeft(l,0)(_ * 10 + _)

  // 6. mergeSorted

  // Fusiona dues llistes ordenades de forma que el resultat queda ordenat

  // Com el nom indica, barregem coses ordenades. Alguns no ho heu considerat així i heu concatenat
  // i ordenat. Si heu entès això, el que no té sentit és definir tota la casuística de si una llista
  // és buida i l'altra no.
  def mergeSorted[A](l1: List[A], l2: List[A])(ord: (A, A) => Boolean): List[A] = 
    (l1, l2) match
      case (Nil, _) => l2
      case (_, Nil) => l1
      case (Cons(a, as), Cons(b, bs)) =>
        if ord(a, b) then Cons(a, mergeSorted(as, l2)(ord))
        else Cons(b, mergeSorted(l1, bs)(ord))

  // 7. Veu una versió del digitsToNum que tracti l'error de que un dels números de la llista no és
  // un dígit -> la capçalera de la funció serà part del problema
  def digitsToNumEither(l: List[Int]): Either[String, Int] =
    foldLeft(l, Right(0): Either[String, Int]) { (acc, a) =>
      if a >= 0 && a <= 9 then acc.map(_ * 10 + a)
      else Left(s" $a dígito no válido")
  }