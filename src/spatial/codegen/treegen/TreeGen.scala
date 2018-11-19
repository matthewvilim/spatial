package spatial.codegen.treegen

import argon._

import spatial.metadata.control._
import spatial.lang._
import spatial.node._
import spatial.util.spatialConfig
import spatial.codegen.naming.NamedCodegen
import spatial.metadata.access._
import spatial.metadata.control._
import spatial.metadata.memory._
import spatial.traversal.AccelTraversal
import spatial.util.modeling.scrubNoise

import scala.collection.mutable.HashMap

case class TreeGen(IR: State) extends AccelTraversal with argon.codegen.Codegen {
  override val ext: String = "html"
  backend = "tree"
  private var ident = 0

  private val swappers = HashMap[Sym[_],Set[Sym[_]]]() // Map from controller to nbufs that it swaps
  override val lang: String = "info"
  override val entryFile: String = "controller_tree.html"

  val memColors = Seq("cce6ff", "ccb6ff", "99ddff", "99ff99", "e6b3cc", "ccffcc", "e0e0d1", "ffcccc",
                      "d1e0e0", "e699ff", "fff7e6", "f2ffcc", "d9b3ff", "cce0ff", "f2e6ff", "ecc6d9") // List of colors I think looks nice

  private val colorMap = HashMap[Sym[_], String]()
  private val nonBufMems = scala.collection.mutable.Set[Sym[_]]()

  override def gen(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case AccelScope(func)     => inAccel{ printControl(lhs,rhs) }
    case _:Control[_] if inHw => printControl(lhs, rhs)
    case _:MemAlloc[_,_] if inHw && (lhs.isSRAM | lhs.isRegFile | lhs.isReg | lhs.isLineBuffer) => logMem(lhs, rhs)
    case _ => rhs.blocks.foreach{blk => gen(blk) }
  }

  def inCell(cellName: String, hasCollapsible: Boolean)(header: => Unit)(collapsible: => Unit): Unit = {
    emit(src"${"  "*ident}<!-- Begin $cellName -->")
    emit(src"""${"  "*ident}<TD>""")
    header
    if (hasCollapsible) inCollapsible{collapsible}
    emit(src"${"  "*ident}</TD>")
    emit(src"${"  "*ident}<!-- End $cellName-->")
  }
  def inTitledCollapsible(hasTitle: Boolean)(title: => Unit)(inside: => Unit): Unit = {
    if (hasTitle) {
      emit("""<TABLE BORDER="1" CELLPADDING="1" CELLSPACING="0"><td>""")
      title
    }
    ident = ident + 1
    
    val coll = "data-role=\"collapsible\""
    emit(s"""${"  "*ident}<div $coll><h4> </h4>""")
    emit(s"""${"  "*ident}<TABLE BORDER="3" CELLPADDING="10" CELLSPACING="10">""")

    inside

    emit(s"""${"  "*ident}</TABLE></div>""")
    ident = ident - 1
    if (hasTitle) emit(s"""${"  "*ident}</TABLE></div>""")
  }

  def inCollapsible(inside: => Unit): Unit = {inTitledCollapsible(false)(())(inside)}

  def assignColor(mem: Sym[_], idx: Option[Int] = None): Unit = {
    val selection = idx.getOrElse(scala.util.Random.nextInt(memColors.length))
    colorMap.getOrElseUpdate(mem, memColors(selection)) 
    ()
  }
  override def quoteConst(tp: Type[_], c: Any): String = c.toString

  protected def link(s: String): String = s"""<a href=IR.html#$s target=_blank>$s</a>"""
  protected def printMem(mem: Sym[_], payload: String*): Unit = {
    val name = mem.name.getOrElse("")
    emit(s"""${"  "*ident}<p><div style="padding: 10px; border: 1px;display:inline-block;background-color: #${colorMap(mem)}"><font size = "1">${link(s"$mem")} (${mem.memName} "$name")""")
    payload.foreach{p => emit(s"<br>$p")}
    emit(s"""${"  "*ident}</font></div></p>""")
  }

  override protected def emitEntry(block: Block[_]): Unit = gen(block)

  def logMem(lhs: Sym[_], rhs: Op[_]): Unit = {
    if (lhs.instance.depth > 1) {
      assignColor(lhs)
      lhs.swappers.foreach{ s => 
        swappers += (s -> (swappers.getOrElse(s, Set()) ++ Set(lhs)))
      }
    }
    else {
      assignColor(lhs, Some(0))
      nonBufMems += lhs
    }
  }

  def printControl(lhs: Sym[_], rhs: Op[_]): Unit = {
    val cchain = lhs.cchains.headOption.map(_.toString)
    val isLeaf = lhs.isInnerControl && lhs.rawChildren.isEmpty
    val line   = lhs.ctx.content.getOrElse("<?:?:?>")

    val isFSM = lhs match {case Op(_: StateMachine[_]) => " FSM"; case _ => ""}
    inCell(src"$lhs", !isLeaf){
      emit(s"""${"  "*ident}<font size = "6">${link(s"${lhs}")}: ${lhs.schedule} $isFSM<font size = "4"> (${lhs.level})</font>""")
      emit(s"""${"  "*ident}<br><font size = "2">${lhs.ctx} <font color="grey">- $line</font></font>""")
      val ii = scrubNoise(lhs.II).toInt
      val lat = scrubNoise(lhs.bodyLatency.sum).toInt
      val attentionII = if (ii > 1) src"<b>II=$ii</b>" else src"II=$ii"
      if (lhs.isInnerControl) emit(s"""${"  "*ident}<p><mark style="border:1px; border-style:solid; border-color:black; padding: 1px; background: #ccc"><font size = "2">Latency=${lat},  ${attentionII}</font></mark></p>""")
      if (swappers.contains(lhs)) {
        inTitledCollapsible(true){
          emit(src"<font size=1>NBuf Connections</font>")
        }{
          swappers(lhs).foreach{mem => 
            printMem(mem)
          }        
        }
      }

      if (cchain.isDefined) emit(s"""${"  "*ident}<br><font size = "1">Counter: ${link(cchain.get)}</font>""")
      emit("")
      print_stream_info(lhs)
    }{
      rhs.blocks.foreach{blk => gen(blk) }
    }
  }

  def print_stream_info(sym: Sym[_]): Unit = {
    val listens = getReadStreams(sym.toCtrl).map{a => s"$a" }
    val pushes  = getWriteStreams(sym.toCtrl).map{a => s"$a" }
    if (listens.nonEmpty || pushes.nonEmpty) {
      emit(s"""${"  "*ident}<div style="border:1px solid black"><font size = "2">Stream Info</font><br><font size = "1"> """)
      if (listens.nonEmpty) emit(s"""<p align="left">----->$listens""")
      if (listens.nonEmpty && pushes.nonEmpty) emit(s"<br>")
      if (pushes.nonEmpty) emit(s"""<p align="right">$pushes----->""")
      emit(s"""${"  "*ident}</font></div>""")
    }
  }



  override def emitHeader(): Unit = {
    val options = {
      (if (!spatialConfig.enableAsyncMem) Seq("SyncMem") else Nil) ++
      (if (spatialConfig.enableRetiming)  Seq("Retimed") else Nil)
    }
    val optionStr = if (options.isEmpty) "None" else options.mkString(", ")

    emit(s"""
  <!DOCTYPE html>
  <html>
  <head>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.css">
  <script src="http://code.jquery.com/jquery-1.11.3.min.js"></script>
  <script src="http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.js"></script>
  </head><body>

    <div data-role="main" class="ui-content" style="overflow-x:scroll;">
      <h2>Controller Diagram for ${spatialConfig.name} (Options: $optionStr)</h2>
      <TABLE BORDER="3" CELLPADDING="10" CELLSPACING="10">
  """)
  }

  override def emitFooter(): Unit = {
    emit("</TABLE>")
    val nbufs = swappers.flatMap{case (_, mems) => mems}.toList.distinct
    emit(s"""<h4> </h4><TABLE BORDER="3" CELLPADDING="10" CELLSPACING="10">""")
    inCell("NBuf Mems", true){
      emit("NBuf Mems")
    }{
      nbufs.toList.map{x => (x, x.constDims.product * x.instance.depth)}.sortBy(_._2).reverse.map(_._1).foreach{mem => 
        val depth = mem.instance.depth
        val dims = mem.constDims
        val pads = mem.getPadding.getOrElse(Seq.fill(dims.length)(0))
        val volume = dims.zip(pads).map{case (d:Int,p:Int) => d+p}.product
        val banks = mem.instance.nBanks
        val alphas = mem.instance.alphas
        val Ps = mem.instance.Ps
        val lca = mem.swappers.head.parent.s.get
        val hasXBarR = if (mem.readers.exists{x => x.port.bufferPort.isDefined && !x.isDirectlyBanked}) "has XBarR" else "<s>has XBarR</s>"
        val hasXBarW = if (mem.writers.exists{x => x.port.bufferPort.isDefined && !x.isDirectlyBanked}) "has XBarW" else "<s>has XBarW</s>"
        printMem(mem, s"lca = ${link(s"$lca")}", s"nBufs = $depth", s"volume = $volume (dims $dims + pads $pads)", s"nBanks = $banks, a = $alphas, p = $Ps", s"$hasXBarR, $hasXBarW")
      }
    }
    inCell("Single-Buffered Mems", true) {
      emit("Single-Buffered Mems")
    } {
      nonBufMems.toList.map{x => (x, x.constDims.product)}.sortBy(_._2).reverse.map(_._1).foreach{mem => 
        val dims = mem.constDims
        val pads = mem.getPadding.getOrElse(Seq.fill(dims.length)(0))
        val volume = dims.zip(pads).map{case (d:Int,p:Int) => d+p}.product
        val banks = mem.instance.nBanks
        val alphas = mem.instance.alphas
        val Ps = mem.instance.Ps
        val hasXBarR = if (mem.readers.exists{x => !x.isDirectlyBanked}) "has XBarR" else "<s>has XBarR</s>"
        val hasXBarW = if (mem.writers.exists{x => !x.isDirectlyBanked}) "has XBarW" else "<s>has XBarW</s>"
        printMem(mem, s"volume = $volume (dims $dims + pads $pads)", s"nBanks = $banks, a = $alphas, p = $Ps", s"$hasXBarR, $hasXBarW")
      }
    }
    emit("</body>")
    emit("</html>")
  }
}
