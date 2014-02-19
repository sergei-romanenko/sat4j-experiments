package experiments.queens

import experiments.solver._

//x00 x01 x02 x03     1  2  3  4
//x10 x11 x12 x13     5  6  7  8
//x20 x21 x22 x23     9 10 11 12
//x30 x31 x32 x33    13 14 15 16

// x_i_j is true iff there is a queen at x_i_j.

case class SATProblemBuilder(n: Int) {

  def varFromSq(i: Int, j: Int): Int = {
    n * i + j + 1
  }

  def sqFromVar(v: Int): Sq = {
    val r = (v - 1) / n
    val c = (v - 1) % n
    (r, c)
  }

  sealed trait Lit {
    def toVar: Int
  }

  case class P(i: Int, j: Int) extends Lit {
    def toVar = varFromSq(i, j)
  }

  case class N(i: Int, j: Int) extends Lit {
    def toVar = -varFromSq(i, j)
  }

  // There is at least one queen per row.
  def atLeastOnePerRow(i: Int): Vector[Lit] =
    for (j <- Vector.range(0, n)) yield P(i, j)

  // There is at least one queen per column.

  def atLeastOnePerCol(j: Int): Vector[Lit] =
    for (i <- Vector.range(0, n)) yield P(i, j)

  // There is no more than one queen per row:
  //     x_i_j implies -x_i_k, if j /= k  

  def atMostOnePerRow(i: Int, j: Int): Seq[Vector[Lit]] = {
    for (k <- 1 to n - 1 - j)
      yield Vector(N(i, j), N(i, j + k))
  }

  // There is no more than one queen per column:
  //     x_i_j implies -x_k_j, if i /= k  

  def atMostOnePerCol(i: Int, j: Int): Seq[Vector[Lit]] = {
    for (k <- 1 to n - 1 - i)
      yield Vector(N(i, j), N(i + k, j))
  }

  def atMostOnePerDiag1(i: Int, j: Int): Seq[Vector[Lit]] = {
    for (k <- 1 to n - 1 - j; if i + k < n)
      yield Vector(N(i, j), N(i + k, j + k))
  }

  def atMostOnePerDiag2(i: Int, j: Int): Seq[Vector[Lit]] = {
    for (k <- 1 to n - 1 - j; if i - k >= 0)
      yield Vector(N(i, j), N(i - k, j + k))
  }

  def buildSATProblem(): List[Vector[Int]] = {
    val clauses = new collection.mutable.ListBuffer[Vector[Lit]]()

    for (i <- 0 to n - 1)
      clauses += atLeastOnePerRow(i)

    for (j <- 0 to n - 1)
      clauses += atLeastOnePerCol(j)

    for (i <- 0 to n - 1; j <- 0 to n - 1) {
      clauses ++= atMostOnePerRow(i, j)
      clauses ++= atMostOnePerCol(i, j)
      clauses ++= atMostOnePerDiag1(i, j)
      clauses ++= atMostOnePerDiag2(i, j)
    }

    val problem: Seq[Vector[Int]] =
      for (c <- clauses) yield for (l <- c) yield l.toVar

    problem.toList
  }

}

case class QSolver(n: Int, timeout: Int = 10) {

  val builder = SATProblemBuilder(n)

  def decodeSolution(s: Vector[Int]): Vector[Sq] =
    for (l <- s; if l > 0) yield builder.sqFromVar(l)

  def getSolutions(): List[Vector[Sq]] = {
    val sat_problem = builder.buildSATProblem()
    val sat_solutions = SATSolver.getAllModels(n * n, sat_problem)

    for (s <- sat_solutions) yield decodeSolution(s)
  }

  def getFirstSolution(): Option[Vector[Sq]] = {
    val sat_problem = builder.buildSATProblem()
    SATSolver.getFirstModel(n * n, sat_problem) map
      { s => decodeSolution(s) }
  }
}
