package edu.iastate.cs.pal

/**
 * Created by danilo04 on 5/31/15.
 */
object AST {
  sealed abstract class Expr

  // Numbers are int or float
  sealed abstract class Number extends Expr
  case class IntNumber(number: Int) extends Number
  case class FloatNumber(number: Float) extends Number


  case class Var(name: String) extends Expr
  case class Quote(expr: Expr) extends Expr
  case class IfTest(test: Expr, conseq: Expr, alt: Expr) extends Expr
  case class Define(varName: Var, expr: Expr) extends Expr
  case class Proc(name: Var, args: List[Expr]) extends Expr


}
