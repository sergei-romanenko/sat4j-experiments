package experiments.queens

//x00 x01 x02 x03     1  2  3  4
//x10 x11 x12 x13     5  6  7  8
//x20 x21 x22 x23     9 10 11 12
//x30 x31 x32 x33    13 14 15 16

// x_i_j is true iff there is a queen at x_i_j.

case class ProblemBuilder(n: Int) {

  def varFromSq(i: Int, j: Int): Int = {
    n * i + j + 1
  }

  def sqFromVar(v: Int): Sq = {
    val r = (v - 1) / n
    val c = (v - 1) - n * r
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
  def atLeastOnePerRow(i: Int): IndexedSeq[Lit] =
    for (j <- 0 to n - 1) yield P(i, j)

  // There is at least one queen per column.

  def atLeastOnePerCol(j: Int): IndexedSeq[Lit] =
    for (i <- 0 to n - 1) yield P(i, j)

  // There is no more than one queen per row:
  //     x_i_j implies -x_i_k, if j /= k  

  def atMostOnePerRow(i: Int, j: Int): Seq[IndexedSeq[Lit]] = {
    for (k <- 1 to n - 1 - j)
      yield Vector(N(i, j), N(i, j + k))
  }

  // There is no more than one queen per column:
  //     x_i_j implies -x_k_j, if i /= k  

  def atMostOnePerCol(i: Int, j: Int): Seq[IndexedSeq[Lit]] = {
    for (k <- 1 to n - 1 - i)
      yield Vector(N(i, j), N(i + k, j))
  }

  def atMostOnePerDiag1(i: Int, j: Int): Seq[IndexedSeq[Lit]] = {
    for (k <- 1 to n - 1 - j; if i + k < n)
      yield Vector(N(i, j), N(i + k, j + k))
  }

  def atMostOnePerDiag2(i: Int, j: Int): Seq[IndexedSeq[Lit]] = {
    for (k <- 1 to n - 1 - j; if i - k >= 0)
      yield Vector(N(i, j), N(i - k, j + k))
  }

  def buildProblem(): Seq[IndexedSeq[Int]] = {
    val clauses = new collection.mutable.ListBuffer[IndexedSeq[Lit]]()

    def appendClause(c: IndexedSeq[Lit]) {
      clauses += c
    }

    def append(cs: Seq[IndexedSeq[Lit]]) {
      for (c <- cs) appendClause(c)
    }

    for (i <- 0 to n - 1)
      appendClause(atLeastOnePerRow(i))

    for (j <- 0 to n - 1)
      appendClause(atLeastOnePerCol(j))

    for (i <- 0 to n - 1; j <- 0 to n - 1) {
      append(atMostOnePerRow(i, j))
      append(atMostOnePerCol(i, j))
      append(atMostOnePerDiag1(i, j))
      append(atMostOnePerDiag2(i, j))
    }
    var problem : Seq[IndexedSeq[Int]] =
      for (c <- clauses) yield
        for (l <- c) yield l.toVar
    problem
  }

  // There is one and only one queen per row and per column
}
