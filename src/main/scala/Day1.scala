import util.Day
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object Day1 extends Day(1) {
  override def solve(): Unit = {

    val leftList =  ArrayBuffer[Int]()
    val rightList =  ArrayBuffer[Int]()

    val scoreMap = HashMap[Int, Int]()

    inputLines.foreach{ line =>
      val parts = line.split("   ")
      val (l,r) = (parts(0).toInt, parts(1).toInt)

      leftList.insert(leftList.lastIndexWhere(_ < l)+1, l)
      rightList.insert(rightList.lastIndexWhere(_ < r)+1, r)
    }
    
    // Part 1
    println(leftList.zip(rightList).map { case (a,b) => (a-b).abs}.sum)

    rightList.foreach{elem => 
      scoreMap.updateWith(elem) {
        case Some(x) => Some(x+1)
        case None => Some(1)
      }
    }

    // Part 2
    println(leftList.map{x => x*scoreMap.getOrElse(x, 0) }.sum)
  }
}