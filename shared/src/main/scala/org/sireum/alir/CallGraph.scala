// #Sireum
/*
 Copyright (c) 2018, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.alir

import org.sireum._
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol._
import org.sireum.lang.symbol.Resolver._
import org.sireum.lang.tipe.TypeHierarchy

object CallGraph {

  type Edge = String

  @datatype class Node(isInObject: B, isVar: B, owner: QName, id: String)

  type Type = Graph[Node, Edge]

  @record class Builder(var g: Type, context: Node) extends AST.MTransformer {

    override def preStmtMethod(o: AST.Stmt.Method): AST.MTransformer.PreResult[AST.Stmt] = {
      g = buildMethod(g, o)
      return AST.MTransformer.PreResult(continu = F, resultOpt = MNone())
    }

    override def preResolvedInfoMethod(o: AST.ResolvedInfo.Method): AST.MTransformer.PreResult[AST.ResolvedInfo] = {
      g = g + context ~> Node(o.isInObject, F, o.owner, o.id)
      return super.preResolvedInfoMethod(o)
    }

    override def preResolvedInfoVar(o: AST.ResolvedInfo.Var): AST.MTransformer.PreResult[AST.ResolvedInfo] = {
      g = g + context ~> Node(o.isInObject, T, o.owner, o.id)
      return super.preResolvedInfoVar(o)
    }
  }

  @pure def build(th: TypeHierarchy): Type = {
    var r = Graph.empty[Node, Edge]

    def buildConstructor(resOpt: Option[AST.ResolvedInfo], stmts: ISZ[AST.Stmt]): Unit = {
      val node: Node = resOpt match {
        case Some(res: AST.ResolvedInfo.Method) => Node(res.isInObject, F, res.owner, res.id)
        case _ => halt("Infeasible")
      }
      var cStmts = ISZ[AST.Stmt]()
      for (stmt <- stmts) {
        stmt match {
          case stmt: AST.Stmt.Var => cStmts = cStmts :+ stmt
          case stmt: AST.Stmt.Expr => cStmts = cStmts :+ stmt
          case _ =>
        }
      }
      r = buildStmts(r, node, cStmts)
    }

    for (info <- th.nameMap.values) {
      info match {
        case info: Info.Method => r = buildMethod(r, info.ast)
        case info: Info.Object => buildConstructor(Some(info.constructorRes), info.ast.stmts)
        case _ =>
      }
    }

    for (info <- th.typeMap.values) {
      info match {
        case info: TypeInfo.Sig =>
          for (m <- info.methods.values) {
            r = buildMethod(r, m.ast)
          }
        case info: TypeInfo.AbstractDatatype =>
          buildConstructor(info.constructorResOpt, info.ast.stmts)
          for (m <- info.methods.values) {
            r = buildMethod(r, m.ast)
          }
        case _ =>
      }
    }

    val refinements = methodRefinements(th)
    for (node <- refinements.nodes.keys) {
      for (subNode <- refinements.childrenOf(node).elements) {
        r = r + node ~> subNode
      }
    }

    return r
  }

  @pure def buildStmts(g: Type, node: Node, stmts: ISZ[AST.Stmt]): Type = {
    val builder = Builder(g, node)
    for (stmt <- stmts) {
      builder.transformStmt(stmt)
    }
    return builder.g
  }

  @pure def buildMethod(g: Type, m: AST.Stmt.Method): Type = {
    if (m.bodyOpt.isEmpty) {
      return g
    }
    val node: Node = m.attr.resOpt match {
      case Some(res: AST.ResolvedInfo.Method) => Node(res.isInObject, F, res.owner, res.id)
      case _ => halt("Infeasible")
    }
    return buildStmts(g, node, m.bodyOpt.get.stmts)
  }

  @pure def methodRefinements(th: TypeHierarchy): Poset[Node] = {
    var r = Poset.empty[Node]
    for (info <- th.typeMap.values) {
      val (refinements, methods, vars): (
        HashMap[String, TypeInfo.Name],
        HashMap[String, Info.Method],
        HashMap[String, Info.Var]
      ) = info match {
        case info: TypeInfo.Sig => (info.refinements, info.methods, HashMap.empty)
        case info: TypeInfo.AbstractDatatype => (info.refinements, info.methods, info.vars)
        case _ => (HashMap.empty, HashMap.empty, HashMap.empty)
      }
      for (p <- refinements.entries) {
        val (id, sup) = p
        val supMResOpt: AST.ResolvedInfo = th.typeMap.get(sup.ids).get match {
          case info: TypeInfo.Sig => info.methods.get(id).get.resOpt.get
          case info: TypeInfo.AbstractDatatype => info.methods.get(id).get.resOpt.get
          case _ => halt("Infeasible")
        }
        val supMRes: Node = supMResOpt match {
          case res: AST.ResolvedInfo.Method => Node(res.isInObject, F, res.owner, res.id)
          case _ => halt("Infeasible")
        }
        vars.get(id) match {
          case Some(v) =>
            val vRes: AST.ResolvedInfo.Var = v.resOpt match {
              case Some(res: AST.ResolvedInfo.Var) => res
              case _ => halt("Infeasible")
            }
            r = r.addParents(Node(vRes.isInObject, T, vRes.owner, vRes.id), ISZ(supMRes))
          case _ =>
            methods.get(id) match {
              case Some(m) =>
                m.resOpt match {
                  case Some(mRes: AST.ResolvedInfo.Method) =>
                    r = r.addParents(Node(mRes.isInObject, F, mRes.owner, mRes.id), ISZ(supMRes))
                  case _ => halt("Infeasible")
                }
              case _ =>
            }
        }
      }
    }
    return r
  }
}
