package edu.iastate.cs.pal

/**
 * Danilo Dominguez Perez
 *
 * Iowa State University
 * Department of Computer Science
 * Program Analysis Laboratory
 *
 */

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * walkeme parser
 */
class Parser extends JavaTokenParsers {
  //Helper parsers
  def id : Parser[String] =
    """[a-zA-Z=*+/<>!\?][a-zA-Z0-9=*+/<>!\?]*""".r ^^ { case s => s }

  def eof = "\\Z".r

  def int: Parser[AST.IntNumber] = wholeNumber  ^^ { case n => AST.IntNumber(n.toInt) }
  def float: Parser[AST.FloatNumber] = floatingPointNumber ^^ { case n => AST.FloatNumber(n.toFloat) }
  def varr = id ^^ { case s => AST.Var(s.toString) }
  def number = int | float
  def quote = "(" ~> "quote" ~> expr <~ ")" ^^ { case e => AST.Quote(e) }
  def define = "(" ~> "define" ~> varr ~ expr <~ ")" ^^ { case v ~ e => AST.Define(v, e) }
  // TODO: for now lets have lambda with at least one parameter
  //def formals: Parser[List[AST.Var]] = varr | "(" ~> rep(varr) <~ ")" |
  def lambda = "(" ~> "lambda" ~> rep1(varr) ~ expr <~ ")" ^^ { case vs ~ e => AST.Lambda(vs, e) }
  def iftest = "(" ~> "if" ~> expr ~ expr ~ expr <~ ")" ^^ { case t ~ c ~ a => AST.IfTest(t, c, a) }
  def proc = "(" ~> expr ~ rep(expr) <~ ")" ^^ { case a ~ as => AST.Proc(a, as) }

  def expr: Parser[AST.Expr] = varr | number | quote | define | lambda | iftest | proc

  def parse(source: String): AST.Expr = parseAll(expr, source).get
}
