package kobe_cs18a.sat


object Example {

  /**
    * HB of SAT の 101 ページの conditioning
    * cnf の A による条件付 (バージョン1)
    */
  def conditioning1(cnf: CNF, lit0: Lit): CNF = {
    val conditioned = for {
      cl0 <- cnf.clauses
      if !cl0.lits.contains(lit0) // 条件1 節の削除
      c = cl0 - ~lit0 // 条件2と3 リテラルの削除
    } yield c
    CNF(conditioned)
  }

  /**
    * HB of SAT の 101 ページの conditioning
    * cnf の A による条件付 (バージョン2)
    */
  def conditioning2(cnf: CNF, lit0: Lit): CNF = {
    val conditioned = for (clause <- cnf.clauses if !clause.lits.contains(lit0)) yield clause - ~lit0
    CNF(conditioned)
  }

  /**
    * HB of SAT の 101 ページの conditioning
    * cnf の A による条件付 (バージョン3)
    */
  def conditioning3(cnf: CNF, lit0: Lit): CNF = {

    var conditioned = Seq.empty[Cl]

    for (clause0 <- cnf.clauses) {
      if (clause0.lits.contains(lit0)) {
        // 何もしない (節の削除)
      } else {
        val clause = clause0 - ~lit0 // ~lit0 の削除
        conditioned = conditioned :+ clause
      }
    }
    CNF(conditioned)
  }


}


case class Lit(name: String, polarity: Boolean) {
  def unary_~ = Lit(name, ! polarity)
  override def toString = name match {
    case "T" => if (polarity) name else "F"
    case _ => if (polarity) name else "~" + name
  }
}

object Lit {
  val True = new Lit("T",true)
  val False = ~True

  def apply(str: String): Lit = str match {
    case manySigns(ss,name) => if (ss.size % 2 == 0) apply(name) else apply("~"+name)
    case "T" | "~F" => True
    case "F" | "~T" => False
    case name => if (name.startsWith("~")) new Lit(name.drop(1),false) else new Lit(name, true)
  }

  val manySigns = """^(~{2,})(.*)$""".r
  // def mkLit(str: String) : Lit =
}

case class Cl(lits: Lit*) {
  def add(that: Cl): Cl = Cl(lits ++ that.lits)
  def add(lit: Lit): Cl = Cl(lits :+ lit) // 末尾追加なので遅い．簡単のため．

  def rm(that: Cl): Cl = Cl(lits.diff(that.lits))
  def rm(lit: Lit): Cl = Cl(lits.diff(Seq(lit)))

  def ++ (that: Cl): Cl = add(that)
  def + (lit: Lit): Cl = add(lit)

  def -- (that: Cl): Cl = rm(that)
  def - (lit: Lit) = rm(lit)

  override def toString = lits.mkString("Cl(",",",")")
}

object Cl {
  def apply(lits: Iterable[Lit]): Cl = new Cl(lits.toSeq: _*)
}


case class CNF(clauses: Cl*) {
  def add(that: CNF): CNF = CNF(clauses ++ that.clauses: _*)
  def add(clause: Cl): CNF = CNF(clauses :+ clause: _*) // 末尾追加なので遅い．簡単のため．

  def rm(that: CNF): CNF = CNF(clauses.diff(that.clauses): _*)
  def rm(cl: Cl): CNF = CNF(clauses.diff(Seq(cl)): _*)

  def ++ (that: CNF): CNF = add(that)
  def + (cl: Cl): CNF = add(cl)

  def -- (that: CNF): CNF = rm(that)
  def - (cl: Cl) = rm(cl)


  def show(): Unit = clauses.foreach{cl =>
    println(cl.lits.mkString(" "))
    }

  override def toString = clauses.mkString("CNF(",",",")")
}

object CNF {
  import scala.io.Source

  def apply(clauses: Iterable[Cl]) = new CNF(clauses.toSeq: _*)

  def load(lines: Seq[String]): CNF = {
    val cnf = for {
      line0 <- lines
      line = line0.trim
      if line != "" && line != "~" && ! line.startsWith("~ ")
      ss = line.split("""\s+""").toSeq
    } yield Cl(ss.map(Lit(_)))
    CNF(cnf)
  }

  def load(fileName: String): CNF =
    load(Source.fromFile(fileName).getLines.toSeq)

}
