import util.Day
import scala.util.Try

def checkXMAS(grid: List[String], pattern: scala.util.matching.Regex = "XMAS".r): Int = {
  grid.map(row => row.sliding(pattern.regex.length).count(pattern.findAllIn(_).length > 0) 
  + row.reverse.sliding(pattern.regex.length).count(pattern.findAllIn(_).length > 0)).sum
}

def rotateInput45Left(grid: List[String]): List[String] = {
  val n = grid.length    
  val m = grid.head.length  
  
  val res = scala.collection.mutable.ListBuffer[String]()
  
  for (d <- -(n - 1) until m) {
    var nextL = ""
    
    for (x <- 0 until n) {
      if (x + d >= 0 && x + d < m) {
        nextL += grid(x)(x + d) 
      }
    }

    res += nextL
  }

  res.toList 
}

def rotateInput45Right(grid: List[String]): List[String] = {
  val n = grid.length     
  val m = grid.head.length  
  
  val res = scala.collection.mutable.ListBuffer[String]()
  
  for (d <- 1 - m until n) { 
    var nextL = ""
    
    for (x <- 0 until n) {
      if (x - d >= 0 && x - d < m) {
        nextL += grid(x)(m - x + d - 1) 
      }
    }

    res += nextL
  }

  res.toList  
}


object Day4 extends Day(4) {
  override def solve(): Unit = {
    val rotatedLeft = rotateInput45Left(inputLines)
    val rotatedRight = rotateInput45Right(inputLines)

    // Part 1
    println(checkXMAS(inputLines) 
    + checkXMAS(inputLines.transpose.map(_.mkString))
    + checkXMAS(rotatedLeft)
    + checkXMAS(rotatedRight))
    
    var xmasCnt = 0

    for (x <- 1 until inputLines.head.size-1) {
      for (y <- 1 until inputLines.size-1) {
        if(inputLines(y)(x) == 'A') {
          if((
                (inputLines(y-1)(x-1) == 'M' && inputLines(y+1)(x+1) == 'S') 
            ||  (inputLines(y-1)(x-1) == 'S' && inputLines(y+1)(x+1) == 'M')
          ) 
          && (
                (inputLines(y+1)(x-1) == 'M' && inputLines(y-1)(x+1) == 'S') 
          ||    (inputLines(y+1)(x-1) == 'S' && inputLines(y-1)(x+1) == 'M')
          )
          ) {
            xmasCnt+=1
          }
        }
      }
    }


    // Part 2
    println(xmasCnt)

    
    //println(inputLines.transpose)
  }
}