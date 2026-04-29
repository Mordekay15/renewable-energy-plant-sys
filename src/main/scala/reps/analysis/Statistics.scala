package reps.analysis

object Statistics {

  final case class Summary(
      count: Int,
      mean: Option[Double],
      median: Option[Double],
      mode:Option[Double],
      range: Option[Double],
      midrange: Option[Double]
  ) {
    def pretty: String =
      s"""count = $count
         |mean = ${fmt(mean)}
         |median = ${fmt(median)}
         |mode = ${fmt(mode)}
         |range = ${fmt(range)}
         |midrange = ${fmt(midrange)}""".stripMargin

    private def fmt(o: Option[Double]): String = o.fold("n/a")(d => f"$d%.3f")
  }

  def mean[A](xs: Seq[A])(implicit num: Numeric[A]): Option[Double] =
    if (xs.isEmpty) None
    else {
      val total = sumRec(xs.toList, num.zero)
      Some(num.toDouble(total) / xs.size.toDouble)
    }

  def median[A](xs: Seq[A])(implicit num: Numeric[A]): Option[Double] =
    if (xs.isEmpty) None
    else {
      val sorted = xs.toVector.sorted(num)
      val n = sorted.size
      if (n % 2 == 1) Some(num.toDouble(sorted(n / 2)))
      else {
        val a = num.toDouble(sorted(n / 2 - 1))
        val b = num.toDouble(sorted(n / 2))
        Some((a + b) / 2.0)
      }
    }

  def mode[A](xs: Seq[A])(implicit num: Numeric[A]): Option[Double] =
    if (xs.isEmpty) None
    else {
      val freq = xs.groupBy(identity).view.mapValues(_.size).toMap
      val maxFreq = freq.valuesIterator.max
      val winners = freq.collect { case (k, v) if v == maxFreq => k }.toList
      val smallest = winners.minBy(num.toDouble)
      Some(num.toDouble(smallest))
    }

  def range[A](xs: Seq[A])(implicit num: Numeric[A]): Option[Double] =
    if (xs.isEmpty) None
    else {
      val (lo, hi) = minMax(xs)
      Some(num.toDouble(hi) - num.toDouble(lo))
    }

  def midrange[A](xs: Seq[A])(implicit num: Numeric[A]): Option[Double] =
    if (xs.isEmpty) None
    else {
      val (lo, hi) = minMax(xs)
      Some((num.toDouble(lo) + num.toDouble(hi)) / 2.0)
    }

  def summary[A](xs: Seq[A])(implicit num: Numeric[A]): Summary =
    Summary(
      count = xs.size,
      mean = mean(xs),
      median = median(xs),
      mode = mode(xs),
      range = range(xs),
      midrange = midrange(xs)
    )

  @scala.annotation.tailrec
  private def sumRec[A](xs: List[A], acc: A)(implicit num: Numeric[A]): A =
    xs match {
      case Nil => acc
      case head :: tail => sumRec(tail, num.plus(acc, head))
    }

  private def minMax[A](xs: Seq[A])(implicit num: Numeric[A]): (A, A) = {
    @scala.annotation.tailrec
    def loop(rest: List[A], lo: A, hi: A): (A, A) = rest match {
      case Nil => (lo, hi)
      case h :: t =>
        val newLo = if (num.lt(h, lo)) h else lo
        val newHi = if (num.gt(h, hi)) h else hi
        loop(t, newLo, newHi)
    }
    val list = xs.toList
    loop(list.tail, list.head, list.head)
  }
}