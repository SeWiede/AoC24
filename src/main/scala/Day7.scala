import util.Day
import scala.util.Try

object Day7 extends Day(7) {
  override def solve(): Unit = {
    // Part 1
    println(inputLines.map(_.split(": ")).map(row => (row(0).toLong, row.tail.mkString(" ").split(" "))).filter{row =>
      Seq.fill(row._2.length-1)(Seq("+", "*")).flatten.combinations(row._2.length-1).flatMap(_.permutations).toList.find{seq => 
      row._2.tail.zip(seq).foldLeft(row._2.head.toLong){case (acc, (num, op)) =>
          op match {
            case "+" => acc + num.toLong
            case "*" => acc * num.toLong
            case _ => acc
          }
        } == row._1
      } != None
   }.map(_._1).sum)
    
    // Part 2
    println(inputLines.map(_.split(": ")).map(row => (row(0).toLong, row.tail.mkString(" ").split(" "))).filter{row =>
      Seq.fill(row._2.length-1)(Seq("+", "*", "||")).flatten.combinations(row._2.length-1).flatMap(_.permutations).toList.find{seq => 
      row._2.tail.zip(seq).foldLeft(row._2.head.toLong){case (acc, (num, op)) =>
          op match {
            case "+" => acc + num.toLong
            case "*" => acc * num.toLong
            case "||" => (acc.toString + num).toLong
            case _ => acc
          }
        } == row._1
      } != None
   }.map(_._1).sum)
  }
}