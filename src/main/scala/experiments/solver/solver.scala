package experiments.solver

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

  def getAllModels(MAXVAR: Int, clauses: Array[Array[Int]]): List[Array[Int]] = {

    val solver: ISolver = SolverFactory.newDefault()

    solver.setTimeout(3600); // 1 hour timeout

    // Prepare the solver to accept MAXVAR variables. MANDATORY.
    solver.newVar(MAXVAR)

    // Not mandatory for SAT solving. MANDATORY for MAXSAT solving.
    solver.setExpectedNumberOfClauses(clauses.size)

    // Feed the solver using Dimacs format , using arrays of int
    // ( best option to avoid dependencies on SAT4J IVecInt )

    // The clause should not contain a 0,
    // only integer ( positive or negative )
    // with absolute values less or equal to MAXVAR.

    // adapt Array to IVecInt
    clauses.foreach(clause => solver.addClause(new VecInt(clause)))

    var models = SATModelIterator(solver)
    models.toList
  }

  def main(args: Array[String]) {

    val solver: ISolver = SolverFactory.newDefault()
    solver.setTimeout(1); // 1 sec timeout

    // (x1 | -x2) & (x2 | -x1)

    val clauses = Array(
      Array(1, -2),
      Array(2, -1))

    var models = getAllModels(2, clauses)

    if (models.isEmpty) {
      printf("UNSAT!")
    } else {
      println("SAT!")
      for (model <- models) {
        for (x <- model) {
          print(x.toString() + " ")
        }
        println()
      }
    }
  }
}
