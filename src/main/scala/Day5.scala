import util.Day
import scala.util.Try
import scala.collection.mutable.HashMap

def getInvalidIdx(curList : List[Int], preReqMap : HashMap[Int, List[Int]], celem : Int) : Int =  {
  curList.zipWithIndex.foldRight(-1){(elem, cidx) => 
    if(preReqMap.getOrElse(elem._1, List()).contains(celem)) elem._2 else cidx
  }
}

object Day5 extends Day(5) {
  override def solve(): Unit = {
    val preReqMap = HashMap[Int, List[Int]]()

    val (pairs, lists) =  inputLines.span(_.contains('|'))

    pairs.map(_.split('|')).foreach{case Array(x,y) => preReqMap.update(y.toInt, (preReqMap.getOrElse(y.toInt, List()) :+ x.toInt))}

    val validlists = lists.tail.map(_.split(',').map(_.toInt)).map( line =>
    line.foldLeft((true, 0,  List[Int]())) { case ((valid, idx, curList), elem) =>
      val insertidx = getInvalidIdx(curList, preReqMap, elem)
      if(insertidx >= 0) {
        (false, idx + 1, (curList.take(insertidx) :+ elem)  ++ curList.drop(insertidx))
      } else{
        (true && valid, idx + 1, curList :+ elem)
      }
    })

    // Part1
    println(validlists.filter(_._1).map(_._3).map(elem => elem(elem.size/2)).sum)
    
    // Part2
    println(validlists.filter(!_._1).map(_._3).map(elem => elem(elem.size/2)).sum)
  }
}