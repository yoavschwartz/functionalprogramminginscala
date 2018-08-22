
//sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

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

object seq {
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.size)
    }
    mean(xs)
      .flatMap((ogMean) =>
        mean(xs.map((x: Double) =>
          math.pow(x - ogMean, 2))))
  }
}

object main {
  def main(args: Array[String]): Unit = {
    //    val result = List.foldLeft(List(3,2,1), Nil: List[Int])(Cons(_,_))

  }
}