// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: _____
//
// AUTHOR1: __________
// TIME1: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: __________
// TIME2: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// This file is compiled with 'sbt compile' and tested with 'sbt test'.
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.    The tests
// will fail for unfnished parts.  Comment such out.

package adpro

// Exercise  1

/* We create OrderedPoint as a trait instead of a class, so we can mix it into
 * Points (this allows to use java.awt.Point constructors without
 * reimplementing them). As constructors are not inherited, We would have to
 * reimplement them in the subclass, if classes not traits are used.  This is
 * not a problem if I mix in a trait construction time. */

trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {

  this: java.awt.Point =>

  override def compare (that: java.awt.Point): Int =  {
    //smaller than 0 this < that
    if (this.x == that.x && this.y == that.y) {
      0
    } else if (this.x < that.x) {
      -1
    } else if (this.x == that.x && this.y < that. y){
      -1
    } else {
      1
    }
  }

}

// Chapter 3


sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f: (A) => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(x) => f(x)
      case Branch(left, right) => g(fold(left)(f)(g),fold(right)(f)(g))
    }
  }

  def size1[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(_ + _ + 1)
  }

  def maximum1(t: Tree[Int]): Int = {
    fold(t)(a => a)(math.max(_,_))
  }

  def map1[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    val first: A => Tree[B] = (a: A) => Leaf(f(a))
    fold(t)(first)(Branch(_, _))
  }

}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(x) => Some(f(x))
      case None => None
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(x) => x
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    if(this != None) this
    else ob
  }

  def filter(p: A => Boolean): Option[A] = {
    this.flatMap(a => if (p(a)) Some(a) else None)
  }

}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // Remember that mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 7 (4.2)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs)
      .flatMap((ogMean) =>
        mean(xs.map((x: Double) =>
          math.pow(x - ogMean, 2))))
  }

  // Exercise 8 (4.3)

  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] = {
    ao.flatMap(a => bo.map(b => f(a,b)))
  }

  // Exercise 9 (4.4)

  def sequence[A] (aos: List[Option[A]]): Option[List[A]] = {
    val optionEmptyList: Option[List[A]] = Some(List())
    val noth: Option[List[A]] = None
    aos.foldRight(optionEmptyList)((opt, accumilator) => opt.flatMap(a => accumilator.map( a::_)).orElse(noth))
  }

  // Exercise 10 (4.5)

  def traverse[A,B] (as: List[A]) (f :A => Option[B]): Option[List[B]] = {
    val optionEmptyList: Option[List[B]] = Some(List())
    val noth: Option[List[B]] = None
    as.foldRight(optionEmptyList)((a: A, accumilator: Option[List[B]]) => f(a).flatMap(a => accumilator.map( a::_)).orElse(noth))
  }

}

object main {
  def main(args: Array[String]): Unit = {
  }
}