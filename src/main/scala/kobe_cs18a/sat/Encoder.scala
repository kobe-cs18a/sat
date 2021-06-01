package kobe_cs18a.sat

abstract class Encoder

case class PairwiseEncoder() extends Encoder {
  // at-most-one と at-least-one clauses を両方出力する

  def encode(c: ExactOne): CNF = {

    val at_most_one_clauses = for {
      Seq(x, y) <- c.xs.combinations(2)
    } yield Cl(~Lit(x), ~Lit(y))

    //    val at_least_one_clause = Cl(c.xs.map(x => Lit(x)))
    val at_least_one_clause = Cl(c.xs.map(Lit(_)))

    CNF(at_most_one_clauses.toSeq :+ at_least_one_clause)
  }

}

case class LadderEncoder() {
  def encode(c: ExactOne): CNF = {
    ???
  }
}

case class BinaryEncoder() {
  def encode(c: ExactOne): CNF = {
    ???
  }
}

object EncoderTest {
  def main(args: Array[String]) = {

    val example = ExactOne(Seq("1", "2", "3"))

    println(s"ExactOne: $example")

    val enc = PairwiseEncoder()

    val cnf = enc.encode(example)

    cnf.show()


  }
}