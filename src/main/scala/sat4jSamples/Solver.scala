package sat4jSamples

import org.sat4j.specs._
import org.sat4j.minisat.SolverFactory
import org.sat4j.tools.ModelIterator
import org.sat4j.core.VecInt

case class SATModelIterator(solver: ISolver)
  extends Iterator[Array[Int]] {

  private val mi: ModelIterator = new ModelIterator(solver);
  private val problem: IProblem = mi
  private var ready = false
  private var isSatisfiable = false

  def hasNext: Boolean = {
    if (!ready) {
      isSatisfiable = problem.isSatisfiable()
      ready = true
    }
    isSatisfiable
  }

  def next(): Array[Int] = {
    if (!hasNext) throw new NoSuchElementException("UNSAT")
    val model: Array[Int] = problem.model();
    ready = false
    model
  }
}

object RunSolver {

  def main(args: Array[String]) {

    val MAXVAR: Int = 5;
    //val NBCLAUSES: Int = 3;

    val solver: ISolver = SolverFactory.newDefault()
    solver.setTimeout(3600); // 1 hour timeout

    // Prepare the solver to accept MAXVAR variables. MANDATORY.
    solver.newVar(MAXVAR)

    // Feed the solver using Dimacs format , using arrays of int
    // ( best option to avoid dependencies on SAT4J IVecInt )

    // The clause should not contain a 0,
    // only integer ( positive or negative )
    // with absolute values less or equal to MAXVAR.

    // (x1 | -x5 | x4) &
    // (-x1 | x5 | x3 | x4) &
    // (-x3 | x4).

    val clauses = Array(
      Array(1, -5, 4),
      Array(-1, 5, 3, 4),
      Array(-3, 4))

    // Not mandatory for SAT solving. MANDATORY for MAXSAT solving.
    //solver.setExpectedNumberOfClauses(clauses.size)

    // adapt Array to IVecInt
    clauses.foreach(clause => solver.addClause(new VecInt(clause)))

    // We are done. Working now on the IProblem interface.

    var models = SATModelIterator(solver)

    if (models.hasNext) {

      for (model <- models) {
        printf("SAT! ")
        for (x <- model) {
          print(x.toString() + " ")
        }
        println()
      }
    } else {
      printf("UNSAT!")
    }
  }
}
