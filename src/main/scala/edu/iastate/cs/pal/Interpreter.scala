package edu.iastate.cs.pal

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
      val value = env.lookup(name).getOrElse(new ValueInt(0)) // TODO: here produce error
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
      (newEnv.update(varName.name, exprValue), new ValueEmpty)
    case AST.Proc(name, args) => (env, new ValueEmpty)
  }
}
