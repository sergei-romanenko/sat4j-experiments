package experiments.solver

import org.scalatest._

import org.sat4j.specs._

import org.sat4j.minisat.SolverFactory

class SolverTest extends FunSpec with Matchers {
  describe("Test1") {
    it ("can execute Sat4j") {

      val solver: ISolver = SolverFactory.newDefault()
      solver.setTimeout(1); // 1 sec timeout

      // (x1 | -x2) & (x2 | -x1)

      val clauses = Array(
        Array(1, -2),
        Array(2, -1))

      var models = RunSolver.getAllModelsAsLists(2, clauses)
      var expected = List(List(-1, -2), List(1, 2))
      
      models should be (expected)
    }
  }
}
