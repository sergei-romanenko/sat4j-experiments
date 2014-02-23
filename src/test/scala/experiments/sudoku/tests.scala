package experiments.sudoku

import org.scalatest._

class SudokuBuilder3Test extends FunSpec with Matchers {
  val builder = SATProblemBuilder(3)
  import builder._

  describe("Cell <-> Int & Lit <-> Int") {
    it("P(1, 1, 1)") {
      P(1, 1, 1).toInt should be(1)
    }
    it("P(1, 3, 1)") {
      P(1, 3, 1).toInt should be(19)
    }
    it("P(1, 1, 3)") {
      P(1, 1, 3).toInt should be(3)
    }
    it("P(2, 1, 1)") {
      P(2, 1, 1).toInt should be(82)
    }
    it("P(2, 3, 7) -> Int") {
      P(2, 3, 7).toInt should be(106)

    }
    it("N -> Int") {
      N(2, 3, 7).toInt should be(-106)

    }
    it("Int -> Cell") {
      cellFromInt(106) should be((2, 3, 7))
    }
  }

  describe("Sub-grids") {
    it("subgridCells(1 , 2)") {
      subgridPs(2, 3) should be(Vector(
        (4, 7), (4, 8), (4, 9),
        (5, 7), (5, 8), (5, 9),
        (6, 7), (6, 8), (6, 9)))
    }
  }
}

class SudokuBuilder2Test extends FunSpec with Matchers {
  val builder = SATProblemBuilder(2)
  import builder._

  describe("Cell sequences") {
    it("rowPs(2)") {
      rowPs(2) should be(Vector(
        (2, 1), (2, 2), (2, 3), (2, 4)))
    }
    it("colPs(3)") {
      rowPs(3) should be(Vector(
        (3, 1), (3, 2), (3, 3), (3, 4)))
    }
  }

  describe("Constraints") {
    it("seqPairs") {
      seqPairs(Vector(1, 2, 3, 4)) should be(Vector(
        (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)))
    }
    it("atLeastOnePerCell(2, 3)") {
      atLeastOnePerCell(2, 3) should be(Vector(
        P(2, 3, 1), P(2, 3, 2), P(2, 3, 3), P(2, 3, 4)))
    }
    it("atMostOnce(1, rowPs(3))") {
      atMostOnce(1, rowPs(3)) should be(Vector(
        Vector(N(3, 1, 1), N(3, 2, 1)),
        Vector(N(3, 1, 1), N(3, 3, 1)),
        Vector(N(3, 1, 1), N(3, 4, 1)),
        Vector(N(3, 2, 1), N(3, 3, 1)),
        Vector(N(3, 2, 1), N(3, 4, 1)),
        Vector(N(3, 3, 1), N(3, 4, 1))))
    }
    it("atMostOnce(4, colPs(2))") {
      atMostOnce(4, colPs(2)) should be(Vector(
        Vector(N(1, 2, 4), N(2, 2, 4)),
        Vector(N(1, 2, 4), N(3, 2, 4)),
        Vector(N(1, 2, 4), N(4, 2, 4)),
        Vector(N(2, 2, 4), N(3, 2, 4)),
        Vector(N(2, 2, 4), N(4, 2, 4)),
        Vector(N(3, 2, 4), N(4, 2, 4))))
    }
    it("atMostOnce(4, subgridPs(2, 2))") {
      atMostOnce(4, subgridPs(2, 2)) should be(Vector(
        Vector(N(3, 3, 4), N(3, 4, 4)),
        Vector(N(3, 3, 4), N(4, 3, 4)),
        Vector(N(3, 3, 4), N(4, 4, 4)),
        Vector(N(3, 4, 4), N(4, 3, 4)),
        Vector(N(3, 4, 4), N(4, 4, 4)),
        Vector(N(4, 3, 4), N(4, 4, 4))))
    }
  }

  describe("Constraints for extended encoding") {
    it("atMostOnePerCell(2, 3)") {
      atMostOnePerCell(2, 3) should be(Vector(
        Vector(N(2, 3, 1), N(2, 3, 2)), Vector(N(2, 3, 1), N(2, 3, 3)),
        Vector(N(2, 3, 1), N(2, 3, 4)),
        Vector(N(2, 3, 2), N(2, 3, 3)), Vector(N(2, 3, 2), N(2, 3, 4)),
        Vector(N(2, 3, 3), N(2, 3, 4))))
    }
    it("atLeastOnce(2, rowPs(3))") {
      atLeastOnce(2, rowPs(3)) should be(Vector(
        P(3, 1, 2), P(3, 2, 2), P(3, 3, 2), P(3, 4, 2)))
    }
    it("atLeastOnce(2, colPs(3))") {
      atLeastOnce(2, colPs(3)) should be(Vector(
        P(1, 3, 2), P(2, 3, 2), P(3, 3, 2), P(4, 3, 2)))
    }
    it("atLeastOnce(4, subgridPs(2, 2))") {
      atLeastOnce(4, subgridPs(2, 2)) should be(Vector(
        P(3, 3, 4), P(3, 4, 4), P(4, 3, 4), P(4, 4, 4)))
    }
  }
}

class SudokuSolverTest extends FunSpec with Matchers {
  describe("Sudoku") {
    it("n == 2 first") {
      val model = SudokuSolver(2, Seq()).getFirstSolution()
      val expected = Set(
        (1, 1, 3), (1, 2, 2), (1, 3, 1), (1, 4, 4),
        (2, 1, 1), (2, 2, 4), (2, 3, 3), (2, 4, 2),
        (3, 1, 2), (3, 2, 1), (3, 3, 4), (3, 4, 3),
        (4, 1, 4), (4, 2, 3), (4, 3, 2), (4, 4, 1))
      model.map(_.toSet) should be(Some(expected))
    }
    it("n == 2") {
      val cells = Seq((1, 1, 3), (2, 4, 2), (3, 2, 1), (3, 3, 4))
      val expected = Set(
        (1, 1, 3), (1, 2, 2), (1, 3, 1), (1, 4, 4),
        (2, 1, 1), (2, 2, 4), (2, 3, 3), (2, 4, 2),
        (3, 1, 2), (3, 2, 1), (3, 3, 4), (3, 4, 3),
        (4, 1, 4), (4, 2, 3), (4, 3, 2), (4, 4, 1))
      val model = SudokuSolver(2, cells).getFirstSolution()
      model.map(_.toSet) should be(Some(expected))
    }
    it("n == 3") {
      val cells = Seq(
        (1, 2, 1), (1, 3, 8), (1, 7, 7),
        (2, 4, 3), (2, 7, 2),
        (3, 2, 7),
        (4, 5, 7), (4, 6, 1),
        (5, 1, 6), (5, 8, 4),
        (6, 1, 3),
        (7, 1, 4), (7, 4, 5), (7, 9, 3),
        (8, 2, 2), (8, 5, 8),
        (9, 8, 6))
      val expected = Set(
        (1, 1, 5), (1, 2, 1), (1, 3, 8), (1, 4, 9), (1, 5, 2), (1, 6, 6), (1, 7, 7), (1, 8, 3), (1, 9, 4),
        (2, 1, 9), (2, 2, 6), (2, 3, 4), (2, 4, 3), (2, 5, 5), (2, 6, 7), (2, 7, 2), (2, 8, 8), (2, 9, 1),
        (3, 1, 2), (3, 2, 7), (3, 3, 3), (3, 4, 1), (3, 5, 4), (3, 6, 8), (3, 7, 6), (3, 8, 5), (3, 9, 9),
        (4, 1, 8), (4, 2, 5), (4, 3, 2), (4, 4, 4), (4, 5, 7), (4, 6, 1), (4, 7, 3), (4, 8, 9), (4, 9, 6),
        (5, 1, 6), (5, 2, 9), (5, 3, 7), (5, 4, 2), (5, 5, 3), (5, 6, 5), (5, 7, 1), (5, 8, 4), (5, 9, 8),
        (6, 1, 3), (6, 2, 4), (6, 3, 1), (6, 4, 8), (6, 5, 6), (6, 6, 9), (6, 7, 5), (6, 8, 2), (6, 9, 7),
        (7, 1, 4), (7, 2, 8), (7, 3, 6), (7, 4, 5), (7, 5, 1), (7, 6, 2), (7, 7, 9), (7, 8, 7), (7, 9, 3),
        (8, 1, 7), (8, 2, 2), (8, 3, 9), (8, 4, 6), (8, 5, 8), (8, 6, 3), (8, 7, 4), (8, 8, 1), (8, 9, 5),
        (9, 1, 1), (9, 2, 3), (9, 3, 5), (9, 4, 7), (9, 5, 9), (9, 6, 4), (9, 7, 8), (9, 8, 6), (9, 9, 2))
      val model = SudokuSolver(3, cells).getFirstSolution()
      model.map(_.toSet) should be(Some(expected))
    }
  }
}

