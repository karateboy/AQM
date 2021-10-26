import scala.math.Ordering.OptionOrdering

object test {
  println("Welcome to the Scala worksheet")
}
def checkA():Option[Boolean] = {
  println("checkA")
  Some(true)
}
def checkB():Option[Boolean] = {
  println("checkB")
  None
}
def checkC():Option[Boolean] = {
  println("checkC")
  Some(false)
}
val seqA:Seq[(Option[Int], ()=>Option[Boolean])] = Seq((Some(1), checkA), (None, checkB), (Some(3), checkC))
implicit val ordering : OptionOrdering[Int] = new OptionOrdering[Int]{
  override def optionOrdering = Ordering[Int]
}
val sorted = seqA.sortBy(_._1).reverse
sorted