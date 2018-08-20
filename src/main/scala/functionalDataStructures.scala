
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => as
    case Cons(h, t) => t
  }
  /** Excercise 3.4*/
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n-1)
      }
    }
  }

  /** Excercise 3.5*/
  def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case Cons(head, tail) => {
          if (!p(head)) l
          else dropWhile(tail)(p)
        }
      }
  }

  /** Excercise 3.6*/
  def init[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, tail) => Cons(h, init(tail))
      }
  }


  /**
    * Excercise 3.3 - replace head of function with different value
    * @param h new head to replace old one
    * @param as list for head replacement
    * @tparam A
    * @return List with new head
    */
  def setHead[A](h: A, as: List[A]): List[A] = as match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /** 3.9 */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0) ((_, acc) => acc+1 )
  }

  /** 3.10 */
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }

  /** 3.11 */
  def sumFoldl(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  /** 3.11 */
  def productFoldl(as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  /** 3.11 */
  def lengthFoldl[A,B](as: List[A]): Int = foldLeft(as, 0)((acc: Int, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

  def foldRightInTermsOfFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as),z)((b,a) => f(a,b))
  }

  def foldLeftInTermsOfFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    //??
    z
  }

  def append[A](as: List[A], a: A): List[A] = {
    val liftedA = List.apply(a)
    foldRight(as, liftedA )(Cons(_,_))
  }

  def append2[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_,_))
  }

  def concat[A](listOfLists: List[List[A]]): List[A] = {
    foldRight(listOfLists, Nil: List[A])(append2(_,_))
  }

  def transformPlus1(ns: List[Int]): List[Int] = {
    foldRight(ns, Nil: List[Int])((n, acc) => Cons(n+1, acc))
  }

  def doublesToStrings(ds: List[Double]): List[String] = {
    foldRight(ds, Nil: List[String])((d, acc) => Cons(d.toString(), acc))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))
  }

  def filter[A](as: List[A])(p: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A]) ((a, z) =>
      if (p(a)) Cons(a, z)
      else z
    )
  }

  def flatMap[A](as: List[A])(f: A => List[A]): List[A] = {
    foldRight(as, Nil: List[A]) ((a: A, z: List[A]) => {
      val aList = f(a)
      append2(aList, z)
    })
  }

  def filterInFlatmap[A](as: List[A])(p: A => Boolean): List[A] = {
      flatMap(as) ((a) => {
        if (p(a)) List(a)
        else Nil
  })
  }

  def addElementsFromLists(l1: List[Int], l2: List[Int]): List[Int] = {
        (l1, l2) match {
          case (Nil, _) => Nil
          case (_, Nil) => Nil
          case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addElementsFromLists(t1, t2))
        }
  }

  def zipWith[A,B](l1: List[A], l2: List[B]): List[(A,B)] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zipWith(t1, t2))
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (Cons(_, _), Nil) => true
      case (Nil, Cons(_, _)) => false
      case (Nil, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) => (hasSubsequence(t1, t2) && (h1 == h2)) || hasSubsequence(t1, sub)
    }
  }

}

object main {
  def main(args: Array[String]): Unit = {
//    val result = List.foldLeft(List(3,2,1), Nil: List[Int])(Cons(_,_))
    println(List.hasSubsequence(List(1,2,3,4,5), List(1,2,3,4,5,6)))
  }
}