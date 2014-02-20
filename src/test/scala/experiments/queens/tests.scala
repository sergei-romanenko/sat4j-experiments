package experiments.queens

import org.scalatest._

class QBuilder4Test extends FunSpec with Matchers {
  val builder = SATProblemBuilder(4)
  import builder._

  describe("Sq <-> Var & Lit <-> Int") {
    it("P -> Int") {
      P(1, 2).toInt should be(7)

    }
    it("N -> Int") {
      N(1, 2).toInt should be(-7)

    }
    it("Var -> Sq") {
      sqFromVar(7) should be((1, 2))
    }
  }

  describe("Problem -> SAT problem") {
    it("atLeastOnePerRow") {
      atLeastOnePerRow(1) should be(
        Vector(P(1, 0), P(1, 1), P(1, 2), P(1, 3)))
    }
    it("atLeastOnePerCol") {
      atLeastOnePerCol(1) should be(
        Vector(P(0, 1), P(1, 1), P(2, 1), P(3, 1)))
    }
    it("atMostOnePerRow") {
      atMostOnePerRow(2, 1) should be(
        Vector(Vector(N(2, 1), N(2, 2)), Vector(N(2, 1), N(2, 3))))
    }
    it("atMostOnePerCol") {
      atMostOnePerCol(1, 2) should be(
        Vector(Vector(N(1, 2), N(2, 2)), Vector(N(1, 2), N(3, 2))))
    }
    it("atMostOnePerDiag1(0, 1)") {
      atMostOnePerDiag1(0, 1) should be(
        Vector(Vector(N(0, 1), N(1, 2)), Vector(N(0, 1), N(2, 3))))
    }
    it("atMostOnePerDiag1(1, 0)") {
      atMostOnePerDiag1(1, 0) should be(
        Vector(Vector(N(1, 0), N(2, 1)), Vector(N(1, 0), N(3, 2))))
    }
    it("atMostOnePerDiag2(2, 0)") {
      atMostOnePerDiag2(2, 0) should be(
        Vector(Vector(N(2, 0), N(1, 1)), Vector(N(2, 0), N(0, 2))))
    }
    it("atMostOnePerDiag2(3, 1)") {
      atMostOnePerDiag2(3, 1) should be(
        Vector(Vector(N(3, 1), N(2, 2)), Vector(N(3, 1), N(1, 3))))
    }
  }
}

class QBuilderTest extends FunSpec with Matchers {
  describe("Problem -> SAT-problem") {
    it("SATProblemBuilder(1).buildSATProblem()") {
      SATProblemBuilder(1).buildSATProblem() should be(
        List(Vector(1), Vector(1)))
    }
    it("SATProblemBuilder(2).buildSATProblem()") {
      SATProblemBuilder(2).buildSATProblem() should be(
        List(Vector(1, 2), Vector(3, 4), Vector(1, 3), Vector(2, 4),
          Vector(-1, -2), Vector(-1, -3), Vector(-1, -4),
          Vector(-2, -4), Vector(-3, -4), Vector(-3, -2)))
    }
    it("SATProblemBuilder(3).buildSATProblem()") {
      SATProblemBuilder(3).buildSATProblem() should be(List(
        Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9), Vector(1, 4, 7),
        Vector(2, 5, 8), Vector(3, 6, 9),
        Vector(-1, -2), Vector(-1, -3), Vector(-1, -4), Vector(-1, -7),
        Vector(-1, -5), Vector(-1, -9), Vector(-2, -3), Vector(-2, -5),
        Vector(-2, -8), Vector(-2, -6), Vector(-3, -6), Vector(-3, -9),
        Vector(-4, -5), Vector(-4, -6), Vector(-4, -7), Vector(-4, -8),
        Vector(-4, -2), Vector(-5, -6), Vector(-5, -8), Vector(-5, -9),
        Vector(-5, -3), Vector(-6, -9), Vector(-7, -8), Vector(-7, -9),
        Vector(-7, -5), Vector(-7, -3), Vector(-8, -9), Vector(-8, -6)))
    }
  }
}

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
    it("n == 5 first") {
      var models = QSolver(5).getFirstSolution()
      var expected = Some(
        Vector((0, 3), (1, 1), (2, 4), (3, 2), (4, 0)))
      models should be(expected)
    }
    it("n == 8") {
      var models = QSolver(8).getSolutions()
      models.size should be(92)
    }
    it("n == 8 first") {
      var models = QSolver(8).getFirstSolution()
      var expected = Some(
        Vector((0, 3), (1, 1), (2, 6), (3, 2), (4, 5), (5, 7), (6, 4), (7, 0)))
      models should be(expected)
    }
    it("n == 9") {
      var models = QSolver(9).getSolutions()
      models.size should be(352)
    }
    it("n == 9 first") {
      var models = QSolver(9).getFirstSolution()
      var expected = Some(
        Vector((0, 4), (1, 6), (2, 8), (3, 3), (4, 1), (5, 7), (6, 5), (7, 2), (8, 0)))
      models should be(expected)
    }
    it("n == 20 first") {
      var models = QSolver(20).getFirstSolution()
      var expected = Some(Vector(
        (0, 12), (1, 10), (2, 1), (3, 11), (4, 9), (5, 18), (6, 0), (7, 17), (8, 14), (9, 7),
        (10, 5), (11, 15), (12, 13), (13, 16), (14, 4), (15, 6), (16, 3), (17, 19), (18, 2), (19, 8)))
      models should be(expected)
    }

  }
}
