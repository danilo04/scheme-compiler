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
case class ValueInt(const: Int) extends Value {
  override def toString() = const.toString
}
case class ValueFloat(const: Float) extends Value {
  override def toString() = const.toString
}
case class ValueClosure(env: Env, params: List[AST.Var], expr: AST.Expr) extends Value {
  override def toString() = "<Lambda>"
}
case class ValueAtom(value: String) extends Value {
  override def toString() = value
}
case class ValueError(errorMsg: String) extends Value {
  override def toString() = "Error: " + errorMsg
}

class Env(val env: Map[String, Value]) {
  def update(varr: String, value: Value) = new Env(env + (varr -> value))
  def lookup(varr: String) = env.get(varr)
  def extend(newEnv: Env): Env = new Env(env ++ newEnv.env)
}

object Env {
  val EMPTY_ENV = new Env(Map[String, Value]())
  def empty = EMPTY_ENV
}