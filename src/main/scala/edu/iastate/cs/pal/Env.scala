package edu.iastate.cs.pal

import edu.iastate.cs.pal.AST


/**
 * Danilo Dominguez Perez
 *
 * Iowa State University
 * Department of Computer Science
 * Program Analysis Laboratory
 *
 */

sealed abstract class Value
case class ValueConst(const: Number) extends Value
case class ValueClosure(env: Env, varr: AST.Var, expr: AST.Expr) extends Value
case class ValueSymbol(value: String) extends Value

class Env(env: Map[AST.Var, Value]) {
  def update(varr: AST.Var, value: Value) = new Env(env + (varr -> value))
  def lookup(varr: AST.Var) = env(varr)
}

object Env {
  def empty = new Env(Map[AST.Var, Value]())
}