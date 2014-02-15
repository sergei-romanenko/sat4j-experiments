package experiments.solver

import org.scalatest._

import org.sat4j.specs._

import org.sat4j.minisat.SolverFactory

class SolverTest extends FunSpec with Matchers {
  describe("Test1") {
    it ("can execute Sat4j") {

      // (x1 | -x2) & (x2 | -x1)

      val clauses = List(
        Vector(1, -2),
        Vector(2, -1))

      var models = SATSolver.getAllModels(2, clauses)
      var expected = List(Vector(-1, -2), Vector(1, 2))
      
      models should be (expected)
    }
  }
}
