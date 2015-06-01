package edu.iastate.cs.pal

/**
 * Danilo Dominguez Perez
 *
 * Iowa State University
 * Department of Computer Science
 * Program Analysis Laboratory
 *
 */

sealed abstract class Value
case class ValueInt(const: Int) extends Value
case class ValueFloat(const: Float) extends Value
case class ValueClosure(env: Env, varr: AST.Var, expr: AST.Expr) extends Value
case class ValueAtom(value: String) extends Value
case class ValueEmpty() extends Value

class Env(env: Map[String, Value]) {
  def update(varr: String, value: Value) = new Env(env + (varr -> value))
  def lookup(varr: String) = env.get(varr)
}

object Env {
  def empty = new Env(Map[String, Value]())
}