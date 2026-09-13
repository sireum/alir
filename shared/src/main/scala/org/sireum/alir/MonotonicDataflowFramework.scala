// #Sireum
package org.sireum.alir

import org.sireum._
import org.sireum.lang.{ast => AST}

object MonotonicDataflowFramework {
  @sig trait Basic[Fact] {
    @pure def cfg: Graph[Z, Unit]
    @pure def isForward: B
    @pure def isLUB: B
    @pure def genGround(g: AST.IR.Stmt.Ground, blockLabel: Z, groundIndex: Z): HashSSet[Fact]
    @pure def killGround(g: AST.IR.Stmt.Ground): HashSSet[Fact]
    @pure def genJump(j: AST.IR.Jump): HashSSet[Fact]
    @pure def killJump(j: AST.IR.Jump): HashSSet[Fact]
    @pure def iota: HashSSet[Fact]
    @pure def init: HashSSet[Fact]

    def compute(body: AST.IR.Body.Basic,
                entrySet: MBox[HashSMap[Z, ISZ[HashSSet[Fact]]]],
                exitSet: MBox[HashSMap[Z, ISZ[HashSSet[Fact]]]]): Unit = {
      if (body.blocks.isEmpty) {
        val m = HashSMap.empty[Z, ISZ[HashSSet[Fact]]] + ControlFlowGraph.exitNode ~> ISZ(iota)
        entrySet.value = m
        exitSet.value = m
        return
      }

      var entryS = entrySet.value
      var exitS = exitSet.value

      def initialize(): Unit = {
        val entries = Buffer.create[(Z, ISZ[HashSSet[Fact]])]()
        for (b <- body.blocks) {
          entries.append(b.label ~> ISZ.create(b.grounds.size + 1, init))
        }
        entries.append(ControlFlowGraph.exitNode ~> ISZ(init))
        val initial = HashSMap.empty[Z, ISZ[HashSSet[Fact]]] ++ entries.toIS
        entryS = initial
        exitS = initial
        if (isForward) {
          val is = entryS.get(body.blocks(0).label).get
          entryS = entryS + body.blocks(0).label ~> is(0 ~> iota)
        } else {
          val values = exitS.get(ControlFlowGraph.exitNode).get
          val boundary = values((values.size - 1) ~> iota)
          entryS = entryS + ControlFlowGraph.exitNode ~> boundary
          exitS = exitS + ControlFlowGraph.exitNode ~> boundary
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

      def forwardBlock(b: AST.IR.BasicBlock): B = {
        val edges = cfg.incoming(b.label)
        val isEntryBlock = b.label == body.blocks(0).label
        var facts: HashSSet[Fact] = if (isEntryBlock) iota else HashSSet.empty[Fact]
        if (edges.isEmpty) {
          if (!isEntryBlock) {
            facts = entryS.get(b.label).get(0)
          }
        } else {
          var start: Z = 0
          if (!isEntryBlock) {
            facts = getExitForward(edges(0).source)
            start = 1
          }
          for (i <- start until edges.size) {
            facts = f(facts, getExitForward(edges(i).source))
          }
        }
        val entries = entryS.get(b.label).get.toMS
        val exits = exitS.get(b.label).get.toMS
        var changed = !facts.isEqual(entries(0))
        entries(0) = facts

        for (i <- 0 until b.grounds.size) {
          val g = b.grounds(i)
          val newExit = (entries(i) -- killGround(g).elements).union(genGround(g, b.label, i))
          if (!newExit.isEqual(exits(i))) {
            changed = T
          }
          exits(i) = newExit
          entries(i + 1) = newExit
        }
        {
          val jumpIndex = b.grounds.size
          val newExit = (entries(jumpIndex) -- killJump(b.jump).elements).union(genJump(b.jump))
          if (!newExit.isEqual(exits(jumpIndex))) {
            changed = T
          }
          exits(jumpIndex) = newExit
        }
        if (changed) {
          entryS = entryS + b.label ~> entries.toIS
          exitS = exitS + b.label ~> exits.toIS
        }
        return changed
      }

      def backwardBlock(b: AST.IR.BasicBlock): B = {
        val edges = cfg.outgoing(b.label)
        var facts = getExitBackward(edges(0).dest)
        for (i <- 1 until edges.size) {
          facts = f(facts, getExitBackward(edges(i).dest))
        }
        val entries = entryS.get(b.label).get.toMS
        val exits = exitS.get(b.label).get.toMS
        val jumpIndex = b.grounds.size
        var changed = !facts.isEqual(exits(jumpIndex))
        exits(jumpIndex) = facts

        {
          val newEntry = (exits(jumpIndex) -- killJump(b.jump).elements).union(genJump(b.jump))
          if (!newEntry.isEqual(entries(jumpIndex))) {
            changed = T
          }
          entries(jumpIndex) = newEntry
          if (jumpIndex > 0) {
            exits(jumpIndex - 1) = newEntry
          }
        }

        for (i <- b.grounds.size - 1 to 0 by -1) {
          val g = b.grounds(i)
          val newEntry = (exits(i) -- killGround(g).elements).union(genGround(g, b.label, i))
          if (!newEntry.isEqual(entries(i))) {
            changed = T
          }
          entries(i) = newEntry
          if (i > 0) {
            exits(i - 1) = newEntry
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
