// #Sireum
package org.sireum.alir

import org.sireum._
import org.sireum.lang.{ast => AST}

object MonotonicDataflowFramework {
  @sig trait Basic[Fact] {
    @pure def cfg: Graph[Z, Unit]
    @pure def isForward: B
    @pure def isLUB: B
    @pure def genGround(g: AST.IR.Stmt.Ground): HashSSet[Fact]
    @pure def killGround(g: AST.IR.Stmt.Ground): HashSSet[Fact]
    @pure def genJump(j: AST.IR.Jump): HashSSet[Fact]
    @pure def killJump(j: AST.IR.Jump): HashSSet[Fact]
    @pure def iota: HashSSet[Fact]
    @pure def init: HashSSet[Fact]

    def compute(body: AST.IR.Body.Basic,
                entrySet: MBox[HashSMap[Z, ISZ[HashSSet[Fact]]]],
                exitSet: MBox[HashSMap[Z, ISZ[HashSSet[Fact]]]]): Unit = {
      var entryS = entrySet.value
      var exitS = exitSet.value

      def initialize(): Unit = {
        var m = HashSMap.empty[Z, ISZ[HashSSet[Fact]]]
        for (b <- body.blocks) {
          m = m + b.label ~> ISZ.create(b.grounds.size + 1, init)
        }
        m = m + ControlFlowGraph.exitNode ~> ISZ(init)
        entryS = m
        exitS = m
        if (isForward) {
          val startNode = body.blocks(0).label
          entryS = entryS + startNode ~> entryS.get(startNode).get(0 ~> iota)
        } else {
          val is = exitS.get(ControlFlowGraph.exitNode).get
          exitS = exitS + ControlFlowGraph.exitNode ~> is((is.size - 1) ~> iota)
        }
      }

      @strictpure def fLUB(facts1: HashSSet[Fact], facts2: HashSSet[Fact]): HashSSet[Fact] = facts1.union(facts2)
      @strictpure def fGLB(facts1: HashSSet[Fact], facts2: HashSSet[Fact]): HashSSet[Fact] = facts1.intersect(facts2)

      val f: (HashSSet[Fact], HashSSet[Fact]) => HashSSet[Fact] = if (isLUB) fLUB _ else fGLB _

      def getExitForward(label: Z): HashSSet[Fact] = {
        val s = exitS.get(label).get
        return s(s.size - 1)
      }

      def getExitBackward(label: Z): HashSSet[Fact] = {
        val s = entryS.get(label).get
        return s(0)
      }

      val getExit: Z => HashSSet[Fact] = if (isForward) getExitForward _ else getExitBackward _

      def forwardBlock(b: AST.IR.BasicBlock): B = {
        val edges = cfg.incoming(b.label)
        var facts = getExit(edges(0).source)
        for (i <- 1 until edges.size) {
          facts = f(facts, getExit(edges(i).source))
        }
        val entries = entryS.get(b.label).get.toMS
        val exits = exitS.get(b.label).get.toMS
        entries(0) = facts

        var changed = F
        for (i <- 0 until b.grounds.size) {
          val g = b.grounds(i)
          val newExit = (entries(i) -- killGround(g).elements).union(genGround(g))
          if (newExit.size != exits(i).size) {
            changed = T
            exits(i) = newExit
            entries(i + 1) = newExit
          }
        }
        {
          val i = b.grounds.size
          val newExit = (entries(i) -- killJump(b.jump).elements).union(genJump(b.jump))
          if (newExit.size != exits(i).size) {
            changed = T
            exits(i) = newExit
          }
        }
        if (changed) {
          entryS = entryS + b.label ~> entries.toIS
          exitS = exitS + b.label ~> exits.toIS
        }
        return changed
      }

      def backwardBlock(b: AST.IR.BasicBlock): B = {
        val edges = cfg.outgoing(b.label)
        var facts = getExit(edges(0).dest)
        for (i <- 1 until edges.size) {
          facts = f(facts, getExit(edges(i).dest))
        }
        val entries = entryS.get(b.label).get.toMS
        val exits = exitS.get(b.label).get.toMS
        exits(b.grounds.size) = facts

        var changed = F

        {
          val i = b.grounds.size
          val newEntry = (exits(i) -- killJump(b.jump).elements).union(genJump(b.jump))
          if (newEntry.size != entries(i).size) {
            changed = T
            entries(i) = newEntry
            if (i > 0) {
              exits(i - 1) = newEntry
            }
          }
        }

        for (i <- b.grounds.size - 1 to 0 by -1) {
          val g = b.grounds(i)
          val newEntry = (exits(i) -- killGround(g).elements).union(genGround(g))
          if (newEntry.size != entries(i).size) {
            changed = T
            entries(i) = newEntry
            if (i > 0) {
              exits(i - 1) = newEntry
            }
          }
        }

        if (changed) {
          entryS = entryS + b.label ~> entries.toIS
          exitS = exitS + b.label ~> exits.toIS
        }
        return changed
      }

      initialize()

      val block: AST.IR.BasicBlock => B = if (isForward) forwardBlock _ else backwardBlock _
      val blockMap: HashMap[Z, AST.IR.BasicBlock] = HashMap ++ (for (b <- body.blocks) yield (b.label, b))

      @pure def bfsLabelsForward(): ISZ[Z] = {
        var r = HashSSet.empty[Z]

        var work = ISZ(body.blocks(0).label)
        r = r ++ work
        while (work.nonEmpty) {
          var next = ISZ[Z]()
          for (label <- work; edge <- cfg.outgoing(label)) {
            if (!r.contains(edge.dest)) {
              next = next :+ edge.dest
              r = r + edge.dest
            }
          }
          work = next
        }

        return r.elements
      }

      @pure def bfsLabelsBackward(): ISZ[Z] = {
        var r = HashSSet.empty[Z]

        var work = ISZ(ControlFlowGraph.exitNode)
        r = r ++ work
        while (work.nonEmpty) {
          var next = ISZ[Z]()
          for (label <- work; edge <- cfg.incoming(label)) {
            if (!r.contains(edge.source)) {
              next = next :+ edge.source
              r = r + edge.source
            }
          }
          work = next
        }

        return r.elements
      }

      val bfsLabels: () => ISZ[Z] = if (isForward) bfsLabelsForward _ else bfsLabelsBackward _

      def work(): B = {
        var changed = F
        for (l <- bfsLabels() if l != ControlFlowGraph.exitNode) {
          val b = blockMap.get(l).get
          if (block(b)) {
            changed = T
          }
        }
        return changed
      }

      var changed = T
      while (changed) {
        changed = work()
      }

      entrySet.value = entryS
      exitSet.value = exitS
    }
  }
}
