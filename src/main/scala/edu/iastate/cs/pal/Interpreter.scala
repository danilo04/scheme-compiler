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


    // TODO: return the environment modified by the expression
    Env.empty
  }
}
