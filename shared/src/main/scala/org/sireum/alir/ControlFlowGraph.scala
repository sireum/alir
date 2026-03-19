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
    g = g * exitNode
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

  /** Compute immediate post-dominators for all blocks.
    *
    * Uses the reverse-CFG dominator algorithm: reverse all edges, then compute
    * dominators with exitNode as root. The dominator of node N in the reverse
    * graph is the post-dominator of N in the original graph.
    *
    * Returns a map from block label to its immediate post-dominator label.
    * The exitNode's ipdom is itself.
    */
  def computeIPostDom(blocks: ISZ[AST.IR.BasicBlock]): HashSMap[Z, Z] = {
    val body = AST.IR.Body.Basic(blocks)
    val cfg = buildBasic(body)

    // Collect all node labels
    var allNodes = ISZ[Z]()
    for (b <- blocks) {
      allNodes = allNodes :+ b.label
    }
    allNodes = allNodes :+ exitNode

    // Build reverse-graph predecessor map.
    // For original edge u → v, the reverse graph has v → u, so v is a
    // predecessor of u in the reverse graph: revPreds[u] += v.
    // For halt/return, exitNode → b.label in reverse: revPreds[b.label] += exitNode.
    var revPreds = HashSMap.empty[Z, ISZ[Z]]
    for (n <- allNodes) {
      revPreds = revPreds + n ~> ISZ[Z]()
    }
    for (b <- blocks) {
      b.jump match {
        case _: AST.IR.Jump.Halt =>
          val preds: ISZ[Z] = revPreds.get(b.label).getOrElse(ISZ())
          revPreds = revPreds + b.label ~> (preds :+ exitNode)
        case _: AST.IR.Jump.Return =>
          val preds: ISZ[Z] = revPreds.get(b.label).getOrElse(ISZ())
          revPreds = revPreds + b.label ~> (preds :+ exitNode)
        case _ =>
          for (target <- b.jump.targets) {
            val preds: ISZ[Z] = revPreds.get(b.label).getOrElse(ISZ())
            revPreds = revPreds + b.label ~> (preds :+ target)
          }
      }
    }

    // Compute dominators on reverse graph (root = exitNode)
    // Using iterative dataflow: dom[n] = {n} ∪ ∩{ dom[p] | p ∈ revPreds[n] }
    // Initialize: dom[exitNode] = {exitNode}, dom[others] = allNodes
    val allSet = HashSet.empty[Z] ++ allNodes
    var dom = HashSMap.empty[Z, HashSet[Z]]
    for (n <- allNodes) {
      if (n == exitNode) {
        dom = dom + n ~> (HashSet.empty[Z] + exitNode)
      } else {
        dom = dom + n ~> allSet
      }
    }

    // Iterate until fixed point
    var changed: B = T
    while (changed) {
      changed = F
      for (n <- allNodes) {
        if (n != exitNode) {
          val preds: ISZ[Z] = revPreds.get(n).getOrElse(ISZ())
          val newDom: HashSet[Z] = if (preds.isEmpty) { HashSet.empty[Z] + n } else {
            var inter: HashSet[Z] = dom.get(preds(0)).getOrElse(allSet)
            var i: Z = 1
            while (i < preds.size) {
              val predDom: HashSet[Z] = dom.get(preds(i)).getOrElse(allSet)
              var filtered = HashSet.empty[Z]
              for (e <- inter.elements) {
                if (predDom.contains(e)) {
                  filtered = filtered + e
                }
              }
              inter = filtered
              i = i + 1
            }
            inter + n
          }
          val oldDom: HashSet[Z] = dom.get(n).getOrElse(allSet)
          if (newDom != oldDom) {
            dom = dom + n ~> newDom
            changed = T
          }
        }
      }
    }

    // Extract immediate post-dominator: for each node, the ipdom is the
    // dominator (in reverse graph) closest to the node (smallest dom set minus self).
    var ipdom = HashSMap.empty[Z, Z]
    for (n <- allNodes) {
      if (n == exitNode) {
        ipdom = ipdom + n ~> exitNode
      } else {
        val doms: HashSet[Z] = dom.get(n).getOrElse(HashSet.empty)
        // ipdom = element of doms \ {n} whose dom set is largest (closest dominator)
        var bestLabel: Z = exitNode
        var bestSize: Z = 0
        for (d <- doms.elements) {
          if (d != n) {
            val dDomSize: Z = dom.get(d).getOrElse(HashSet.empty).size
            if (dDomSize > bestSize) {
              bestLabel = d
              bestSize = dDomSize
            }
          }
        }
        ipdom = ipdom + n ~> bestLabel
      }
    }
    return ipdom
  }

}