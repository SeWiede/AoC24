import util.Day

def isSafe(line: List[Int]): Boolean = {
  line.sliding(2).forall {
    case List(a, b) => a > b && (a - b).abs <= 3
    case _ => false
  } || line.sliding(2).forall {
    case List(a, b) => a < b && (b - a).abs <= 3
    case _ => false
  }
}

object Day2 extends Day(2) {
  override def solve(): Unit = {
    var safe = inputLines.map(_.split(" ").map(_.toInt).toList).toList.count { 
      isSafe(_)
    }

    // Part 1
    println(safe)

    safe = inputLines.map(_.split(" ").map(_.toInt).toList).toList.count { line =>
      isSafe(line) || line.indices.exists { i => 
        isSafe(line.take(i) ++ line.drop(i+1))
      }
    }

    // Part 2
    println(safe)
  }
}