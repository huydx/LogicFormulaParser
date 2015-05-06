import scala.util.parsing.combinator._

trait Ops {
  type Op = (Boolean, Boolean) => Boolean
  def and(x: Boolean, y: Boolean) = x & y
  def or(x: Boolean, y: Boolean) = x | y
}


class Logical extends Ops with JavaTokenParsers {
  def expr: Parser[Any] = rep(boolExpr~operator) ^^ {
    case boolExprs =>
      boolExprs.foreach {
        case value ~ op =>
          println(value)
          println(op)
      }
  }

  def bool: Parser[Boolean] = ("TRUE" | "FALSE") ^^ (_.toBoolean)
  def boolExpr: Parser[Any] = bool | "("~expr~")"
  def operator: Parser[(Boolean, Boolean) => Boolean] = ("AND" | "OR") ^^ {
    case "AND" => and
    case "OR" => or
  }
}

object ParseExpr extends Logical {
  def main(args: Array[String]) {
    println("input : "+ args(0))
    val result = parseAll(expr, args(0))
  }
}