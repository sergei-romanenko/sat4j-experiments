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
      subgridPs(1, 2) should be(Vector(
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
    it("atMostOnePerRow(1, 3)") {
      atMostOnePerRow(1, 3) should be(Vector(
        Vector(N(3, 1, 1), N(3, 2, 1)),
        Vector(N(3, 1, 1), N(3, 3, 1)),
        Vector(N(3, 1, 1), N(3, 4, 1)),
        Vector(N(3, 2, 1), N(3, 3, 1)),
        Vector(N(3, 2, 1), N(3, 4, 1)),
        Vector(N(3, 3, 1), N(3, 4, 1))))
    }
    it("atMostOnePerCol(4, 2)") {
      atMostOnePerCol(4, 2) should be(Vector(
        Vector(N(1, 2, 4), N(2, 2, 4)),
        Vector(N(1, 2, 4), N(3, 2, 4)),
        Vector(N(1, 2, 4), N(4, 2, 4)),
        Vector(N(2, 2, 4), N(3, 2, 4)),
        Vector(N(2, 2, 4), N(4, 2, 4)),
        Vector(N(3, 2, 4), N(4, 2, 4))))
    }
    it("atMostOnePerSubgrid(4, 2, 2)") {
      atMostOnePerSubgrid(4, 2, 2) should be(Vector(
        Vector(N(3, 3, 4), N(3, 4, 4)),
        Vector(N(3, 3, 4), N(4, 3, 4)),
        Vector(N(3, 3, 4), N(4, 4, 4)),
        Vector(N(3, 4, 4), N(4, 3, 4)),
        Vector(N(3, 4, 4), N(4, 4, 4)),
        Vector(N(4, 3, 4), N(4, 4, 4))))
    }
  }
  //  describe("Problems") {
  //    it("buildSATProblem()") {
  //      buildSATProblem() should be (List())
  //    }
  //  }
}

class SudokuSolverTest extends FunSpec with Matchers {
  describe("Sudoku") {
    it("n == 2") {
      var models = SudokuSolver(2).getFirstSolution()
      var expected = Set()
      models.toSet should be(expected)
    }

  }
}