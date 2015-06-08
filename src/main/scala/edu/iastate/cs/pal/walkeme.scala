package edu.iastate.cs.pal

/**
 * Danilo Dominguez Perez
 *
 * Iowa State University
 * Department of Computer Science
 * Program Analysis Laboratory
 *
 */

/**
 * Main program
 */
object walkeme {

  def main(args: Array[String]) = {
    println("walkeme compiler")
    // TODO: create global environment with +, - functions
    Interpreter.eval("(define r 10) (* pi (* r r)))", Env.empty)
  }
}
