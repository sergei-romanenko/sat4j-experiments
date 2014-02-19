package experiments.sudoku

import experiments.solver._

case class SATProblemBuilder(n: Int) {
  val nn = n * n

  def intFromCell(r: Int, c: Int, d: Int): Int = {
    ((r - 1) * nn + (c - 1)) * nn + d
  }

  def cellFromInt(v: Int): Cell = {
    val rc = (v - 1) / nn
    val d = (v - 1) % nn + 1
    val r = rc / nn + 1
    val c = rc % nn + 1
    (r, c, d)
  }

  sealed trait Lit {
    def toInt: Int
  }

  case class P(r: Int, c: Int, d: Int) extends Lit {
    def toInt = intFromCell(r, c, d)
  }

  case class N(r: Int, c: Int, d: Int) extends Lit {
    def toInt = -intFromCell(r, c, d)
  }

  // Sets of cells.

  def rowPs(r: Int): IndexedSeq[(Int, Int)] =
    for (c <- 1 to nn) yield (r, c)

  def colPs(c: Int): IndexedSeq[(Int, Int)] =
    for (r <- 1 to nn) yield (r, c)

  // Note that sub-grids are indexed starting at 0.

  def subgridPs(i: Int, j: Int): IndexedSeq[(Int, Int)] =
    for (r <- 1 to n; c <- 1 to n)
      yield (n * i + r, n * j + c)

  def seqPairs[A](s: IndexedSeq[A]): IndexedSeq[(A, A)] = {
    for (i <- 0 until s.size - 1; j <- i + 1 until s.size)
      yield (s(i), s(j))
  }

  def atMostOne(d: Int, ps: IndexedSeq[Pos]) =
    for (((r1, c1), (r2, c2)) <- seqPairs(ps))
      yield Vector(N(r1, c1, d), N(r2, c2, d))

  // There is at least one number in each entry.

  def atLeastOnePerCell(r: Int, c: Int): Vector[Lit] =
    for (d <- Vector.range(1, nn + 1)) yield P(r, c, d)

  //  Each number appears at most once in each row, column and sub-grid.

  def atMostOnePerRow(d: Int, r: Int): IndexedSeq[Vector[Lit]] =
    atMostOne(d, rowPs(r))

  def atMostOnePerCol(d: Int, c: Int): IndexedSeq[Vector[Lit]] =
    atMostOne(d, colPs(c))

  // Each number appears at most once in each n*n sub-grid.
  def atMostOnePerSubgrid(d: Int, i: Int, j: Int): IndexedSeq[Vector[Lit]] =
    atMostOne(d, subgridPs(i - 1, j - 1))

  def buildSATProblem(): List[Vector[Int]] = {
    val clauses = new collection.mutable.ListBuffer[Vector[Lit]]()

    for (r <- 1 to nn; c <- 1 to nn)
      clauses += atLeastOnePerCell(r, c)

    for (d <- 1 to n; r <- 1 to nn)
      clauses ++= atMostOnePerRow(d, r)

    for (d <- 1 to n; c <- 1 to nn)
      clauses ++= atMostOnePerCol(d, c)

    for (d <- 1 to n; i <- 1 to n; j <- 1 to n)
      clauses ++= atMostOnePerSubgrid(d, i, j)

    val problem: Seq[Vector[Int]] =
      for (c <- clauses) yield for (l <- c) yield l.toInt

    problem.toList
  }
}

case class SudokuSolver(n: Int, timeout: Int = 10) {

  val nn = n * n
  val nv = nn * nn * nn
  
  val builder = SATProblemBuilder(n)

  def decodeSolution(s: Vector[Int]): Vector[Cell] =
    for (l <- s; if l > 0) yield builder.cellFromInt(l)

  def getSolutions(): List[Vector[Cell]] = {
    val sat_problem = builder.buildSATProblem()
    val sat_solutions = SATSolver.getAllModels(nv, sat_problem)

    for (s <- sat_solutions) yield decodeSolution(s)
  }

  def getFirstSolution(): Option[Vector[Cell]] = {
    val sat_problem = builder.buildSATProblem()
    SATSolver.getFirstModel(nv, sat_problem) map
      { s => println(s); decodeSolution(s) }
  }
}
