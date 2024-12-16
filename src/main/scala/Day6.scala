import util.Day
import scala.util.Try
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

def stepUp(grid: ArrayBuffer[ArrayBuffer[(Char, Int, Int)]], coord: (Int, Int)): (((Int, Int), Boolean), Int) = {
  val steps = (0 to coord._2).reverse
    .takeWhile(y => y > 0 && grid(y-1)(coord._1)._1 != '#')
    .foldLeft(0, 0)((acc, y) => {
      val (us, s) = acc
      val (char, occ, dn) = grid(y)(coord._1)
      grid(y)(coord._1) = (char, occ+1, dn)
      if(occ == 0) (us+1,s+1) else (us, s+1)
    })

  val newY = coord._2 - steps._2
  val newCoord = ((coord._1, newY), newY == 0) 

  (newCoord, steps._1) 
}

def stepDown(grid: ArrayBuffer[ArrayBuffer[(Char, Int, Int)]], coord: (Int, Int)): (((Int, Int), Boolean), Int) = {
  val steps = (coord._2 until grid.size)
    .takeWhile(y => y < grid.size - 1 && grid(y+1)(coord._1)._1 != '#')
    .foldLeft(0, 0)((acc, y) => {
      val (us, s) = acc
      val (char, occ, dn) = grid(y)(coord._1)
      grid(y)(coord._1) = (char, occ+1, dn)
      if(occ == 0) (us+1,s+1) else (us, s+1)
    })

  val newY = coord._2 + steps._2
  val newCoord = ((coord._1, newY), newY == grid.size - 1) 

  (newCoord, steps._1) 
}

def stepRight(grid: ArrayBuffer[ArrayBuffer[(Char, Int, Int)]], coord: (Int, Int)): (((Int, Int), Boolean), Int) = {
  val steps = (coord._1 until grid.head.size)
    .takeWhile(x => x < grid.head.size - 1 && grid(coord._2)(x+1)._1 != '#')
    .foldLeft(0, 0)((acc, x) => {
      val (us, s) = acc
      val (char, occ, dn) = grid(coord._2)(x)
      grid(coord._2)(x) = (char, occ+1, dn)
      if(occ == 0) (us+1,s+1) else (us, s+1)
    })

  val newX = coord._1 + steps._2
  val newCoord = ((newX, coord._2), newX == grid.head.size - 1) 

  (newCoord, steps._1) 
}

def stepLeft(grid: ArrayBuffer[ArrayBuffer[(Char, Int, Int)]], coord: (Int, Int)): (((Int, Int), Boolean), Int) = {
  val steps = (0 to coord._1).reverse
    .takeWhile(x => x > 0 && grid(coord._2)(x-1)._1 != '#')
    .foldLeft(0, 0)((acc, x) => {
      val (us, s) = acc
      val (char, occ, dn) = grid(coord._2)(x)
      grid(coord._2)(x) = (char, occ+1, dn)
      if(occ == 0) (us+1,s+1) else (us, s+1)
    })

  val newX = coord._1 - steps._2
  val newCoord = ((newX, coord._2), newX == 0) 

  (newCoord, steps._1) 
}

object Day6 extends Day(6) {
  override def solve(): Unit = {
    val grid = ArrayBuffer(inputLines.map(_.map(c => (c, 0, 0))).map(row => ArrayBuffer(row: _*)): _*)
      
    var soldier = (grid.zipWithIndex.collectFirst{
      case (row, y) if row.exists(_._1 == '^') => (row.indexWhere(_._1 == '^'), y)
    }.get, false)

    println("soldier is at " + soldier)

    var steps = 0
    var dir = 0
    while(!soldier._2) {
      if(dir == 0) {
        val (newSoldier, newSteps) = stepUp(grid, (soldier._1))
        soldier = newSoldier
        steps += newSteps
        dir = 1
        println("walked " + newSteps + " up")
        println("soldier is at " + soldier)
      } else if (dir == 1) {
        val (newSoldier, newSteps) = stepRight(grid, (soldier._1))
        soldier = newSoldier
        steps += newSteps
        dir = 2
        println("walked " + newSteps + " right")
        println("soldier is at " + soldier)
      } else if (dir == 2) {
        val (newSoldier, newSteps) = stepDown(grid, (soldier._1))
        soldier = newSoldier
        steps += newSteps
        dir = 3
        println("walked " + newSteps + " down")
        println("soldier is at " + soldier)
      } else if (dir == 3) {
        val (newSoldier, newSteps) = stepLeft(grid, (soldier._1))
        soldier = newSoldier
        steps += newSteps
        dir = 0
        println("walked " + newSteps + " left")
        println("soldier is at " + soldier)
      }
    }

    // Part 1
    println(steps + 1)
    
       
  }
}