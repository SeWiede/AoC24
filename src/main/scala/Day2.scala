import util.Day

object Day2 extends Day(2):
  override def solve(): Unit =
    val input = inputLines.map(l => l.split(" ").map(_.toInt))
    
    //Part 1
    println(input.count(safe))

    //Part 2
    println(input.map(withTolerance).map(l => l.find(safe)).count(_.isDefined))

  def safe(seq: Array[Int]): Boolean = safe(seq.sliding(2).map(l => (l.head, l(1))).toArray, (l, r) => Math.signum(l - r) == Math.signum(seq.head - seq(1)))

  def safe(seq: Array[(Int, Int)], step: (Int, Int) => Boolean): Boolean = seq.forall((l, r) => step(l, r) && Math.abs(l - r) >= 1 && Math.abs(l - r) <= 3)

  def withTolerance(seq: Array[Int]): List[Array[Int]] =
    seq.indices.map(i => seq.patch(i, Nil, 1)).toList