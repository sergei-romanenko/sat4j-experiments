package experiments.queens

import org.sat4j.specs._
import org.sat4j.minisat.SolverFactory
import org.sat4j.tools.ModelIterator
import org.sat4j.core.VecInt

import experiments.solver._

//x00 x01 x02 x03     1  2  3  4
//x10 x11 x12 x13     5  6  7  8
//x20 x21 x22 x23     9 10 11 12
//x30 x31 x32 x33    13 14 15 16

object Worksheet {

  val builder = ProblemBuilder(4)                 //> builder  : experiments.queens.ProblemBuilder = ProblemBuilder(4)

  val p12 = builder.P(1, 2)                       //> p12  : experiments.queens.Worksheet.builder.P = P(1,2)
  val var12 = p12.toVar                           //> var12  : Int = 7
  val sq12 = builder.sqFromVar(var12)             //> sq12  : (Int, Int) = (1,2)

  val opr1 = builder.atLeastOnePerRow(1)          //> opr1  : IndexedSeq[experiments.queens.Worksheet.builder.Lit] = Vector(P(1,0)
                                                  //| , P(1,1), P(1,2), P(1,3))
  val opc1 = builder.atLeastOnePerCol(1)          //> opc1  : IndexedSeq[experiments.queens.Worksheet.builder.Lit] = Vector(P(0,1)
                                                  //| , P(1,1), P(2,1), P(3,1))

  val fe: IndexedSeq[Int] =
    for (j <- 1 to 5) yield j                     //> fe  : IndexedSeq[Int] = Vector(1, 2, 3, 4, 5)

  val sr21 = builder.atMostOnePerRow(2, 1)        //> sr21  : Seq[IndexedSeq[experiments.queens.Worksheet.builder.Lit]] = Vector(V
                                                  //| ector(N(2,1), N(2,2)), Vector(N(2,1), N(2,3)))
  val sc12 = builder.atMostOnePerCol(1, 2)        //> sc12  : Seq[IndexedSeq[experiments.queens.Worksheet.builder.Lit]] = Vector(V
                                                  //| ector(N(1,2), N(2,2)), Vector(N(1,2), N(3,2)))

  val du01 = builder.atMostOnePerDiag1(0, 1)      //> du01  : Seq[IndexedSeq[experiments.queens.Worksheet.builder.Lit]] = Vector(V
                                                  //| ector(N(0,1), N(1,2)), Vector(N(0,1), N(2,3)))
  val du10 = builder.atMostOnePerDiag1(1, 0)      //> du10  : Seq[IndexedSeq[experiments.queens.Worksheet.builder.Lit]] = Vector(V
                                                  //| ector(N(1,0), N(2,1)), Vector(N(1,0), N(3,2)))
  val dd20 = builder.atMostOnePerDiag2(2, 0)      //> dd20  : Seq[IndexedSeq[experiments.queens.Worksheet.builder.Lit]] = Vector(V
                                                  //| ector(N(2,0), N(1,1)), Vector(N(2,0), N(0,2)))
  val dd31 = builder.atMostOnePerDiag2(3, 1)      //> dd31  : Seq[IndexedSeq[experiments.queens.Worksheet.builder.Lit]] = Vector(V
                                                  //| ector(N(3,1), N(2,2)), Vector(N(3,1), N(1,3)))
  val problem1 =
    ProblemBuilder(1).buildProblem().toList       //> problem1  : List[IndexedSeq[Int]] = List(Vector(1), Vector(1))
  val problem2 =
    ProblemBuilder(2).buildProblem().toList       //> problem2  : List[IndexedSeq[Int]] = List(Vector(1, 2), Vector(3, 4), Vector(
                                                  //| 1, 3), Vector(2, 4), Vector(-1, -2), Vector(-1, -3), Vector(-1, -4), Vector(
                                                  //| -2, -4), Vector(-3, -4), Vector(-3, -2))
  val problem4 =
    ProblemBuilder(4).buildProblem().toList       //> problem4  : List[IndexedSeq[Int]] = List(Vector(1, 2, 3, 4), Vector(5, 6, 7
                                                  //| , 8), Vector(9, 10, 11, 12), Vector(13, 14, 15, 16), Vector(1, 5, 9, 13), V
                                                  //| ector(2, 6, 10, 14), Vector(3, 7, 11, 15), Vector(4, 8, 12, 16), Vector(-1,
                                                  //|  -2), Vector(-1, -3), Vector(-1, -4), Vector(-1, -5), Vector(-1, -9), Vecto
                                                  //| r(-1, -13), Vector(-1, -6), Vector(-1, -11), Vector(-1, -16), Vector(-2, -3
                                                  //| ), Vector(-2, -4), Vector(-2, -6), Vector(-2, -10), Vector(-2, -14), Vector
                                                  //| (-2, -7), Vector(-2, -12), Vector(-3, -4), Vector(-3, -7), Vector(-3, -11),
                                                  //|  Vector(-3, -15), Vector(-3, -8), Vector(-4, -8), Vector(-4, -12), Vector(-
                                                  //| 4, -16), Vector(-5, -6), Vector(-5, -7), Vector(-5, -8), Vector(-5, -9), Ve
                                                  //| ctor(-5, -13), Vector(-5, -10), Vector(-5, -15), Vector(-5, -2), Vector(-6,
                                                  //|  -7), Vector(-6, -8), Vector(-6, -10), Vector(-6, -14), Vector(-6, -11), Ve
                                                  //| ctor(-6, -16), Vector(-6, -3), Vector(-7, -8), Vector(-7, -11), Vector(-7, 
                                                  //| -15), Vector(-7, -12), 
                                                  //| Output exceeds cutoff limit.

  var models1 = RunSolver.getAllModelsAsLists(1, problem1)
                                                  //> models1  : List[List[Int]] = List(List(1))
  var models2 = RunSolver.getAllModelsAsLists(4, problem2)
                                                  //> models2  : List[List[Int]] = List()
  var models4 = RunSolver.getAllModelsAsLists(16, problem4)
                                                  //> models4  : List[List[Int]] = List(List(-1, -2, 3, -4, 5, -6, -7, -8, -9, -1
                                                  //| 0, -11, 12, -13, 14, -15, -16), List(-1, 2, -3, -4, -5, -6, -7, 8, 9, -10, 
                                                  //| -11, -12, -13, -14, 15, -16))
}