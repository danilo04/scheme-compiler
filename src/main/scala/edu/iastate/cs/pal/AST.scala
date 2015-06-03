package edu.iastate.cs.pal

/**
 * Danilo Dominguez Perez
 *
 * Iowa State University
 * Department of Computer Science
 * Program Analysis Laboratory
 *
 */
object AST {
  sealed abstract class Expr

  // Numbers are int or float
  sealed abstract class Number extends Expr
  case class IntNumber(number: Int) extends Number {
    override def toString() = number.toString
  }
  case class FloatNumber(number: Float) extends Number {
    override def toString() = number.toString
  }

  case class Var(name: String) extends Expr {
    override def toString() = name
  }
  case class Quote(expr: Expr) extends Expr {
    override def toString() = "(quote " + expr.toString + ")"
  }
  case class IfTest(test: Expr, conseq: Expr, alt: Expr) extends Expr {
    override def toString() = "(if " + test.toString + " " + conseq.toString + " " + alt.toString + ")"
  }
  case class Define(varName: Var, expr: Expr) extends Expr {
    override def toString() = "(define " + varName.toString() + " " + expr.toString + ")"
  }
  case class Lambda(args: List[Var], body: Expr) extends Expr {
    override def toString() = "(lambda " + args.toString + " " + body.toString + ")"
  }
  case class Proc(app: Expr, args: List[Expr]) extends Expr {
    override def toString() = "(" + app.toString() + " " + args.toString + ")"
  }
}
