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

  // Sets of positions.

  def rowPs(r: Int): IndexedSeq[(Int, Int)] =
    for (c <- 1 to nn) yield (r, c)

  def colPs(c: Int): IndexedSeq[(Int, Int)] =
    for (r <- 1 to nn) yield (r, c)

  def subgridPs(i: Int, j: Int): IndexedSeq[(Int, Int)] =
    for (r <- 1 to n; c <- 1 to n)
      yield (n * (i - 1) + r, n * (j - 1) + c)

  def seqPairs[A](s: IndexedSeq[A]): IndexedSeq[(A, A)] = {
    for (i <- 0 until s.size - 1; j <- i + 1 until s.size)
      yield (s(i), s(j))
  }

  def atMostOnce(d: Int, ps: IndexedSeq[Pos]): IndexedSeq[Vector[Lit]] =
    for (((r1, c1), (r2, c2)) <- seqPairs(ps))
      yield Vector(N(r1, c1, d), N(r2, c2, d))

  def atLeastOnce(d: Int, ps: IndexedSeq[Pos]): Vector[Lit] =
    for ((r, c) <- ps.toVector) yield P(r, c, d)

  // There is at least one number in each entry.

  def atLeastOnePerCell(r: Int, c: Int): Vector[Lit] =
    for (d <- Vector.range(1, nn + 1))
      yield P(r, c, d)

  // There is at most one number in each entry.

  def atMostOnePerCell(r: Int, c: Int): IndexedSeq[Vector[Lit]] =
    for ((d1, d2) <- seqPairs(Vector.range(1, nn + 1)))
      yield Vector(N(r, c, d1), N(r, c, d2))

  def buildSATProblem(cells: Seq[Cell]): List[Vector[Int]] = {
    val clauses = new collection.mutable.ListBuffer[Vector[Lit]]()

    // The minimal encoding.

    // Known cells.

    for ((r, c, d) <- cells)
      clauses += Vector(P(r, c, d))

    // There is at least one number in each entry.

    for (r <- 1 to nn; c <- 1 to nn)
      clauses += atLeastOnePerCell(r, c)

    //  Each number appears at most once in each row.

    for (d <- 1 to nn; r <- 1 to nn)
      clauses ++= atMostOnce(d, rowPs(r))

    //  Each number appears at most once in each column.

    for (d <- 1 to nn; c <- 1 to nn)
      clauses ++= atMostOnce(d, colPs(c))

    // Each number appears at most once in each n*n sub-grid.

    for (d <- 1 to nn; i <- 1 to n; j <- 1 to n)
      clauses ++= atMostOnce(d, subgridPs(i, j))

    // The extended encoding includes all the clauses of the minimal encoding,
    // as well as the following constraints.

    // There is at most one number in each entry.

    for (r <- 1 to nn; c <- 1 to nn)
      clauses ++= atMostOnePerCell(r, c)

    // Each number appears at least once in each row.

    for (d <- 1 to nn; r <- 1 to nn)
      clauses += atLeastOnce(d, rowPs(r))

    // Each number appears at least once in each column.

    for (d <- 1 to nn; c <- 1 to nn)
      clauses += atLeastOnce(d, colPs(c))

    // Each number appears at least once in each n*n sub-grid.

    for (d <- 1 to nn; i <- 1 to n; j <- 1 to n)
      clauses += atLeastOnce(d, subgridPs(i, j))

    // Literals to integers.

    val problem: Seq[Vector[Int]] =
      for (c <- clauses) yield for (l <- c) yield l.toInt

    problem.toList
  }
}

case class SudokuSolver(
  n: Int,
  cells: Seq[Cell],
  timeout: Int = 10) {

  val nn = n * n
  val nv = nn * nn * nn

  val builder = SATProblemBuilder(n)

  def decodeSolution(s: Vector[Int]): Vector[Cell] =
    for (l <- s; if l > 0) yield builder.cellFromInt(l)

  def getSolutions(): List[Vector[Cell]] = {
    val sat_problem = builder.buildSATProblem(cells)
    val sat_solutions = SATSolver.getAllModels(nv, sat_problem)

    for (s <- sat_solutions) yield decodeSolution(s)
  }

  def getFirstSolution(): Option[Vector[Cell]] = {
    val sat_problem = builder.buildSATProblem(cells)
    SATSolver.getFirstModel(nv, sat_problem) map
      { s => decodeSolution(s) }
  }
}
