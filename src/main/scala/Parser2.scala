import scala.util.parsing.combinator._

abstract class Value {
  def eval():Double
}
case class Number(n:Double) extends Value {
  def eval():Double = n
}
case class Binomial(op: String, v1:Value, v2:Value) extends Value {
  def eval():Double = op match {
    case "*" => v1.eval() * v2.eval()
    case "/" => v1.eval() / v2.eval()
    case "+" => v1.eval() + v2.eval()
    case "-" => v1.eval() - v2.eval()
  }
}
case class Rest(op:String, v:Value)
case class Formula(v:Value, rest:List[Rest]) extends Value {
  def eval():Double = rest.foldLeft(v.eval()) {
    (r:Double, rest:Rest) => Binomial(rest.op, Number(r), rest.v).eval()
  }
}
case class Paren(e:Value) extends Value {
  def eval():Double = e.eval()
}

class ArithmeticParser extends JavaTokenParsers {
  def multiDiv: Parser[Rest] = ("*"|"/")~factor ^^ { case op~v => Rest(op, v) }
  def addSub: Parser[Rest]   = ("+"|"-")~term   ^^ { case op~v => Rest(op, v) }
  def expr: Parser[Value] = term~rep(addSub)     ^^ { case f1~rest => Formula(f1, rest) }
  def term: Parser[Value] = factor~rep(multiDiv) ^^ { case f1~rest => Formula(f1, rest) }
  def factor: Parser[Value] = (
    floatingPointNumber ^^ { case n => Number(n.toDouble) }
      | "-"~>floatingPointNumber ^^ { case n => Number(n.toDouble * -1.0) }
      | "("~>expr<~")" ^^ { case e => Paren(e) }
    )

  def parse(e:String): Double = {
    parseAll(expr, e).get.eval
  }
}

object Parser2 extends App {
  val parser = new ArithmeticParser
  val e = args(0)
  val result = parser.parse(e)
  println(result)
}