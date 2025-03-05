// #Sireum
package org.sireum.alir

import org.sireum._
import org.sireum.lang.{ast => AST}

object ControlFlowGraph {
  val exitNode: Z = -1

  def buildBasic(body: AST.IR.Body.Basic): Graph[Z, Unit] = {
    var g = Graph.emptyMulti[Z, Unit]
    for (b <- body.blocks) {
      g = g * b.label
    }
    g = g * -1
    for (b <- body.blocks) {
      b.jump match {
        case _: AST.IR.Jump.Halt => g = g + b.label ~> exitNode
        case _: AST.IR.Jump.Return => g = g + b.label ~> exitNode
        case _ =>
          for (target <- b.jump.targets) {
            g = g + b.label ~> target
          }
      }
    }
    return g
  }
}