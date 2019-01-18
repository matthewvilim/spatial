package spatial.tests.feature.dense

import spatial.dsl._

@spatial class TPCH1 extends SpatialTest {
  override def runtimeArgs: Args = NoArgs

/*
SELECT
    l_returnflag,
    l_linestatus,
    sum(l_quantity) as sum_qty,
    sum(l_extendedprice) as sum_base_price,
    sum(l_extendedprice * (1 - l_discount)) as sum_disc_price,
    sum(l_extendedprice * (1 - l_discount) * (1 + l_tax)) as sum_charge,
    avg(l_quantity) as avg_qty,
    avg(l_extendedprice) as avg_price,
    avg(l_discount) as avg_disc,
    count(*) as count_order
FROM
    lineitem
WHERE
    l_shipdate <= date '1998-12-01' - interval '90' day
GROUP BY
    l_returnflag,
    l_linestatus
ORDER BY
    l_returnflag,
    l_linestatus;
*/

  def main(args: Array[String]): Unit = {

		type T = I32

		val nLineItem = 100
    val lShipdate = DRAM[T](nLineItem)
    val lShipdatePred = 100

    val lShipdateFilterIndices = DRAM[I32](nLineItem)
    val lShipdateFilterCount = Reg[T](0)

    val lReturnFlag = DRAM[T](nLineItem)

		Accel {
			val shipdateFilterIn = FIFO[T](4)
			val shipdateFilterOut = FIFO[T](4)
			Stream {
				shipdateFilterIn load lShipdate(0::nLineItem)
        Foreach(nLineItem par 4) { i =>
					val sel = shipdateFilterIn.deq() <= lShipdatePred
          val c = compress(pack(sel, i))
          shipdateFilterOut.enq(c._2, c._1)
        }
				lShipdateFilterIndices(0::nLineItem) store shipdateFilterOut
			}
		}
	}

}
