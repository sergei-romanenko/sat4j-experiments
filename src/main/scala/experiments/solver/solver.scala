package experiments.solver

import org.sat4j.specs._
import org.sat4j.minisat.SolverFactory
import org.sat4j.tools.ModelIterator
import org.sat4j.core.VecInt

case class SATModelIterator(solver: ISolver)
  extends Iterator[Vector[Int]] {

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

  def next(): Vector[Int] = {
    if (!hasNext) throw new NoSuchElementException("UNSAT")
    val model: Array[Int] = problem.model();
    ready = false
    model.toVector
  }
}

object SATSolver {

  def getModelIterator(
    maxvar: Int,
    clauses: List[Vector[Int]],
    timeout: Int = 10): SATModelIterator = {

    val solver: ISolver = SolverFactory.newDefault()

    solver.setTimeout(timeout)

    // Prepare the solver to accept MAXVAR variables. MANDATORY.
    solver.newVar(maxvar)

    // Not mandatory for SAT solving. MANDATORY for MAXSAT solving.
    solver.setExpectedNumberOfClauses(clauses.size)

    // Feed the solver using Dimacs format , using arrays of int
    // ( best option to avoid dependencies on SAT4J IVecInt )

    // The clause should not contain a 0,
    // only integer ( positive or negative )
    // with absolute values less or equal to MAXVAR.

    // adapt Array to IVecInt
    clauses.foreach(clause => solver.addClause(new VecInt(clause.toArray)))

    SATModelIterator(solver)
  }

  def getAllModels(
    maxvar: Int,
    clauses: List[Vector[Int]],
    timeout: Int = 10): List[Vector[Int]] = {

    var models = getModelIterator(maxvar, clauses, timeout)
    models.toList
  }

  def getFirstModel(
    maxvar: Int,
    clauses: List[Vector[Int]],
    timeout: Int = 10): Option[Vector[Int]] = {

    var models = getModelIterator(maxvar, clauses, timeout)
    if (!models.hasNext) None else Some(models.next)
  }

  def main(args: Array[String]) {

    val solver: ISolver = SolverFactory.newDefault()

    // (x1 | -x2) & (x2 | -x1)

    val clauses = List(
      Vector(1, -2),
      Vector(2, -1))

    var models = getAllModels(2, clauses, timeout = 1)

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
