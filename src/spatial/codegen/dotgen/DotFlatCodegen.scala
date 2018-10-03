package spatial.codegen.dotgen

import argon._
import scala.collection.mutable
import utils.io.files._
import sys.process._
import scala.language.postfixOps

trait DotFlatCodegen extends DotCodegen {

  override def entryFile: String = s"Main.$ext"

  // Remove all dot files. First dot gen
  //override def clearGen(): Unit = super.clearGen()

  override protected def gen(lhs: Sym[_], rhs: Op[_]): Unit = {
    currScope.addNode(lhs)
    if (rhs.blocks.nonEmpty) {
      open(src"subgraph cluster_${lhs} {")
      emit(src"""${graphAttr(lhs).map { case (k,v) => s"$k=$v" }.mkString(" ")}""")
      //strMeta(lhs)
      if (inputs(lhs).nonEmpty) emitNode(lhs)
      rhs.binds.filter(_.isBound).foreach{ b =>
        currScope.addNode(b)
        emitNode(b)
        emitEdge(lhs, b)
      }
      rhs.blocks.foreach(ret)
      close(src"}")
    } else {
      emitNode(lhs)
    }
  }

  override def graphAttr(lhs:Sym[_]):Map[String,String] = lhs match {
    case lhs if blocks(lhs).nonEmpty => 
      super.graphAttr(lhs) + ("URL" -> s""""${s"$lhs.html"}"""")
    case lhs => super.graphAttr(lhs)
  }

}
