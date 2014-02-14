package experiments.solver

object Worksheet {
  val x = 1                                       //> x  : Int = 1
  val y = Seq(1, 2)                               //> y  : Seq[Int] = List(1, 2)

  val clauses =
    collection.mutable.ListBuffer[Int]()          //> clauses  : scala.collection.mutable.ListBuffer[Int] = ListBuffer()

  clauses += 1                                    //> res0: experiments.solver.Worksheet.clauses.type = ListBuffer(1)
  clauses += 2                                    //> res1: experiments.solver.Worksheet.clauses.type = ListBuffer(1, 2)

  for (clause <- clauses)
    println(clause)                               //> 1
                                                  //| 2

  val iseq : IndexedSeq[Int] = IndexedSeq(1, 2)   //> iseq  : IndexedSeq[Int] = Vector(1, 2)
  val arr : Array[Int] = iseq.toArray             //> arr  : Array[Int] = Array(1, 2)
}