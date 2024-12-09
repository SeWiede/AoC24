import util.Day
import scala.util.Try

object Day3 extends Day(3) {
  override def solve(): Unit = {
    // Part 1
    println(inputLines.flatMap(_.split("mul\\(").toList).map(_.split("\\)")(0).split(",")).filter(_.size == 2).map(_.map(_.trim).collect{case s if Try(s.toInt).isSuccess => s.toInt}).toList.filter(_.size == 2).map{case Array(a,b) => a * b}.toList.sum)

    // Part 2
    println(inputLines.mkString("\n").split("don't\\(\\)").zipWithIndex
    .foldLeft("") { (curStr, partIndex) => 
      val (part, index) = partIndex
      if (index == 0)
        part
      else if (part.indexOf("do()") >= 0) {
        curStr + part.substring(part.indexOf("do()") + 4)
      } else {
        curStr
      }
    }.split("mul\\(").toList.map(_.split("\\)")(0).split(",")).filter(_.size == 2) .map(_.map(_.trim).collect{case s if Try(s.toInt).isSuccess => s.toInt}).toList.filter(_.size == 2).map{case Array(a,b) => a * b}.toList.sum)
  }
}