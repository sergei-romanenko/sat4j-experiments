package experiments.queens

import org.scalatest._

//import org.sat4j.specs._
//
//import org.sat4j.minisat.SolverFactory

class QSolverTest extends FunSpec with Matchers {
  describe("n-Queen") {
    it("n == 1") {
      var models = QSolver(1).getSolutions()
      var expected = Set(Vector((0, 0)))
      models.toSet should be(expected)
    }
    it("n == 2") {
      var models = QSolver(2).getSolutions()
      var expected = Set()
      models.toSet should be(expected)
    }
    it("n == 3") {
      var models = QSolver(3).getSolutions()
      var expected = Set()
      models.toSet should be(expected)
    }
    it("n == 4") {
      var models = QSolver(4).getSolutions()
      var expected = Set(
        Vector((0, 2), (1, 0), (2, 3), (3, 1)),
        Vector((0, 1), (1, 3), (2, 0), (3, 2)))
      models.toSet should be(expected)
    }
    it("n == 5") {
      var models = QSolver(5).getSolutions()
      var expected = Set(
        Vector((0, 3), (1, 1), (2, 4), (3, 2), (4, 0)),
        Vector((0, 2), (1, 4), (2, 1), (3, 3), (4, 0)),
        Vector((0, 3), (1, 0), (2, 2), (3, 4), (4, 1)),
        Vector((0, 4), (1, 2), (2, 0), (3, 3), (4, 1)),
        Vector((0, 4), (1, 1), (2, 3), (3, 0), (4, 2)),
        Vector((0, 1), (1, 4), (2, 2), (3, 0), (4, 3)),
        Vector((0, 2), (1, 0), (2, 3), (3, 1), (4, 4)),
        Vector((0, 1), (1, 3), (2, 0), (3, 2), (4, 4)),
        Vector((0, 0), (1, 3), (2, 1), (3, 4), (4, 2)),
        Vector((0, 0), (1, 2), (2, 4), (3, 1), (4, 3)))
      models.toSet should be(expected)
    }
    it("n == 8") {
      var models = QSolver(8).getSolutions()
      var expected = List()
      models.size should be(92)
    }
    it("n == 9") {
      var models = QSolver(9).getSolutions()
      var expected = List()
      models.size should be(352)
    }

  }
}
