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
/*case class ValueClosure(env: Env, params: List[AST.Var], expr: AST.Expr) extends Value {
  override def toString() = "<Lambda>"
}*/
case class ValueClosure(env: Env, f: (Env, List[Value]) => Option[(Env, Value)]) extends Value {
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
  def global = {
    val globalEnv = Map[String, Value](
      "+" -> new ValueClosure(EMPTY_ENV, add),
      "-" -> new ValueClosure(EMPTY_ENV, minus),
      "*" -> new ValueClosure(EMPTY_ENV, mult),
      "/" -> new ValueClosure(EMPTY_ENV, div)
    )

    new Env(globalEnv)
  }

  def add(env: Env, params: List[Value]) = {
    arithmetic(env, params, _+_)
  }
  def minus(env: Env, params: List[Value]) = {
    arithmetic(env, params, _-_)
  }
  def mult(env: Env, params: List[Value]) = {
    arithmetic(env, params, _*_)
  }
  def div(env: Env, params: List[Value]) = {
    arithmetic(env, params, _/_)
  }

  def arithmetic(env: Env, params: List[Value], f: (Float, Float) => Float) = {
    val values = params.map({
      case ValueInt(n) => Some(n.toFloat)
      case ValueFloat(n) => Some(n)
      case _ => None
    })
    if (values.exists(!_.isDefined)) None
    else Some((env, new ValueFloat(values.flatten.fold[Float](0)(f(_, _)))))
  }


}

