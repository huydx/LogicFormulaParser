import scala.util.parsing.combinator._

abstract class BoolExpression {
  def eval(): Boolean
}

case class EntityValue(e: BoolExpression) extends BoolExpression {
  override def eval() = e.eval()
}
case class BoolValue(n: Boolean) extends BoolExpression {
  override def eval() = n
}
case class NotOperatorValue(value: BoolExpression) extends BoolExpression {
  override def eval() = {
    !value.eval()
  }
}
case class BinaryOperatorValue(op: String, v1: BoolExpression, v2: BoolExpression) extends BoolExpression {
  override def eval() = op match {
    case "and" => v1.eval() & v2.eval()
    case "or"  => v1.eval() | v2.eval()
  }
}

case class UnaryOperatorValue(op: String, value: BoolExpression)

case class FormulaValue(startValue: BoolExpression, list: List[UnaryOperatorValue]) extends BoolExpression {
  override def eval() = {
    list.foldLeft(startValue.eval()) {
      (v: Boolean, op: UnaryOperatorValue) => BinaryOperatorValue(op.op, BoolValue(v), op.value).eval()
    }
  }
}


class Logical extends JavaTokenParsers {
  def expr: Parser[BoolExpression] = term ~ rep(op) ^^ {
    case v~list => FormulaValue(v, list)
  }

  def op: Parser[UnaryOperatorValue] = ("and" | "or") ~ term ^^ {case(op~v) => UnaryOperatorValue(op,v)}

  def term: Parser[BoolExpression] = factor | notfactor
  def notfactor: Parser[BoolExpression] = "not" ~ factor ^^ {case(op~v) => NotOperatorValue(v)}
  def factor: Parser[BoolExpression] = (
    bool ^^ {case e => BoolValue(e)}
    | "("~>expr<~")" ^^ {case e => EntityValue(e)}
  )

  def bool: Parser[Boolean] = ("true" | "false") ^^ {case(n) => n.toBoolean}
}

object ParseExpr extends Logical {
  def main(args: Array[String]) {
    println("input : "+ args(0))
    val result = parseAll(expr, args(0))
    println(result.get.eval())
  }
}