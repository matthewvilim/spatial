package spatial.tests.feature.dense

import spatial.dsl._

@spatial class TPCH1 extends SpatialTest {
  import spatial.lib.Sort

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
    val lQuantity = DRAM[T](nLineItem)
    val lExtendedPrice = DRAM[T](nLineItem)
    val lDiscount = DRAM[T](nLineItem)
    val lTax = DRAM[T](nLineItem)

    val sumQuantity = DRAM[T](nLineItem)
    val sumBasePrice = DRAM[T](nLineItem)
    val sumDiscPrice = DRAM[T](nLineItem)
    val sumCharge = DRAM[T](nLineItem)
    val avgQuantity = DRAM[T](nLineItem)
    val avgPrice = DRAM[T](nLineItem)
    val avgDisc = DRAM[T](nLineItem)
    val countOrder = DRAM[T](nLineItem)

    val filterRowIdx = DRAM[T](nLineItem * 2)
    val returnFlagFilterRowVal = DRAM[T](nLineItem * 2)

    val lReturnFlag = DRAM[T](nLineItem)

		Accel {
      val filterCount = Reg[T](0)

			Stream {

        val filterIdx = FIFO[T](4)
        val shipdateVal = FIFO[T](4)
        val gatherIdx = FIFO[T](4)
        val returnFlagVal = FIFO[T](4)

				shipdateVal load lShipdate(0::nLineItem)
        Foreach(0 until nLineItem) { i =>
          val lShipdatePred = 1000
					val sel = shipdateVal.deq() <= lShipdatePred
          filterCount := filterCount + mux(sel, 1, 0)
          filterIdx.enq(i, sel)
          gatherIdx.enq(i, sel)
        }

				filterRowIdx(0::filterCount) store filterIdx
        returnFlagVal gather lReturnFlag(gatherIdx, filterCount)
        returnFlagFilterRowVal(0::filterCount) store returnFlagVal
			}

      // TODO: runtime count
      //Sort.mergeSort(filterRowIdx, returnFlagFilterRowVal, 1, 2, 128)

      Stream {
        val bucketVal = Reg[T](0)
        val bucketSize = Reg[T](0)

        val sumQuantityAccum = Reg[T](0)
        val sumBasePriceAccum = Reg[T](0)

        val groupByIdx = FIFO[T](4)
        val groupByVal = FIFO[T](4)

        val quantityVal = FIFO[T](4)
        val extendedPriceVal = FIFO[T](4)

        val sumQuantityVal = FIFO[T](4)
        val avgQuantityVal = FIFO[T](4)
        val sumBasePriceVal = FIFO[T](4)
        val avgPriceVal = FIFO[T](4)
        val countOrderVal = FIFO[T](4)

        groupByIdx load filterRowIdx(0::filterCount)
        groupByVal load returnFlagFilterRowVal(0::filterCount)
        quantityVal gather lQuantity(groupByIdx, filterCount)
        extendedPriceVal gather lExtendedPrice(groupByIdx, filterCount)

        Foreach(0 until filterCount) { i =>
          val newBucket = groupByVal.peek() != bucketVal.value
          bucketVal := mux(newBucket, groupByVal.peek(), bucketVal.value)
          bucketSize := mux(newBucket, 0, bucketSize.value + 1)

          sumQuantityAccum := mux(newBucket, 0, sumQuantityAccum) + quantityVal.deq()
          sumBasePriceAccum := mux(newBucket, 0, sumBasePriceAccum) + extendedPriceVal.deq()

          sumQuantityVal.enq(sumQuantityAccum.value, newBucket)
          avgQuantityVal.enq(sumQuantityAccum.value / bucketSize.value, newBucket)
          sumBasePriceVal.enq(sumBasePriceAccum.value, newBucket)
          avgPriceVal.enq(sumBasePriceAccum.value / bucketSize.value, newBucket)
          countOrderVal.enq(bucketSize.value, newBucket)
        }
        sumQuantity(0::filterCount) store sumQuantityVal
        avgQuantity(0::filterCount) store avgQuantityVal
        sumBasePrice(0::filterCount) store sumBasePriceVal
        avgPrice(0::filterCount) store avgPriceVal
        countOrder(0::filterCount) store countOrderVal
      }
		}
	}

}
