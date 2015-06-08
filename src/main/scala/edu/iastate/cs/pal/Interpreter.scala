package edu.iastate.cs.pal

import scala.collection
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable

/**
 * Danilo Dominguez Perez
 *
 * Iowa State University
 * Department of Computer Science
 * Program Analysis Laboratory
 *
 */
object Interpreter {
  def eval(source: String, env: Env): Env = {
    val parser = new Parser()
    val expr = parser.parse(source)

    val (newEnv, value) = evalExpr(expr, env)
    println(value)

    newEnv
  }

  private def evalExpr(expr: AST.Expr, env: Env): (Env, Value) = expr match {
    case AST.IntNumber(number) => (env, new ValueInt(number))
    case AST.FloatNumber(number) => (env, new ValueFloat(number))
    case AST.Var(name) =>
      val value = env.lookup(name).getOrElse(new ValueError("Variable with name " + name + " does not exists."))
      (env, value)
    case AST.Quote(expr) => (env, new ValueAtom(expr.toString))
    case AST.IfTest(test, conseq, alt) =>
      val (newEnv, testValue) = evalExpr(test, env)
      val testBool = testValue match {
        case ValueInt(number) => number > 0
        case ValueFloat(number) => number > 0f
        case ValueAtom(value) => !value.isEmpty
        case _ => true
      }

      if (testBool) evalExpr(conseq, newEnv)
      else evalExpr(alt, newEnv)
    case AST.Define(varName, expr) =>
      val (newEnv, exprValue) = evalExpr(expr, env)
      (newEnv.update(varName.name, exprValue), exprValue)
    case AST.Proc(app, args) =>
      val (envApp, valueApp) = evalExpr(app, env)
      valueApp match {
        case ValueClosure(env, f) =>
          // evaluate the arguments
          val (valuesArgs, envArgs) = evalArgs(args, envApp)
          f(envArgs, valuesArgs) match {
            case Some(result) => result
            case _ => (env, new ValueError("There was an error executing procedure <" + app.toString + ">"))
          }
        case _ => (env, new ValueError("Value " + valueApp + " is not a function."))
      }
    case AST.Lambda(args, body) =>
      val f: (Env, List[Value]) => Option[(Env, Value)] = (envExec, params) => {
        bindVars(args, params, env) match {
          case Some(envBinding) => Some(evalExpr(body, envExec.extend(envBinding)))
          case _ => None
        }
      }
      (env, new ValueClosure(env, f))
  }

  private def bindVars(vars : List[AST.Var], values: List[Value], env: Env): Option[Env] = (vars, values) match {
    case ((varr) :: (restVars), (value) :: (restValues)) =>
      val newEnv = env.update(varr.name, value)
      bindVars(restVars, restValues, newEnv)
    case _ => None
  }

  private def evalArgs(args: List[AST.Expr], env: Env): (List[Value], Env) = args match {
    case (arg) :: (rest) =>
      val (envArg, valueArg) = evalExpr(arg, env)
      val (valuesArgs, envFinal) = evalArgs(rest, envArg)
      (valueArg :: valuesArgs, envFinal)
    case _ => (Nil, env)
  }
}
