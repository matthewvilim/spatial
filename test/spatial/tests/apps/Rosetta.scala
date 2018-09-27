package spatial.tests.Rosetta

import spatial.dsl._
import spatial.targets._

@spatial class Rendering3D extends SpatialTest {
	override def runtimeArgs = "0 3192 0"
	//override val target = Zynq
	// struct size of power of 2
	@struct case class triangle3D(x0 : UInt8, y0 : UInt8, z0 : UInt8, 
								  x1 : UInt8, y1 : UInt8, z1 : UInt8,
								  x2 : UInt8, y2 : UInt8, z2 : UInt8, 
								  padding1 : UInt32, padding2 : UInt16, padding3 : UInt8)

	@struct case class triangle2D(x0 : UInt8, y0 : UInt8,
								  x1 : UInt8, y1 : UInt8,
								  x2 : UInt8, y2 : UInt8, 
								  z  : UInt8)

	@struct case class CandidatePixel(x : UInt8, y : UInt8, z : UInt8, color : UInt8)

	@struct case class Pixel(x : UInt8, y : UInt8, color : UInt8)

	def coloringFB(size_pixels 		: Int,
				   pixels 			: FIFO[Pixel],
				   frame_buffer		: SRAM2[UInt8]) : Unit = {

		//println(" " + size_pixels)
		Foreach(size_pixels by 1){ i =>
			val curr_pixel = pixels.deq()
			frame_buffer(curr_pixel.y.to[I32], curr_pixel.x.to[I32]) = curr_pixel.color
		}
	}

	def zculling(fragments 	: FIFO[CandidatePixel],
				 size_fragment : Int,
				 pixels 	: FIFO[Pixel],
				 z_buffer 	: SRAM2[UInt8]) : Int = {

		val pixel_reg = Reg[Int](0)
		Pipe { pixel_reg := 0.to[Int] }

		Foreach(size_fragment by 1) { p =>
			val curr_fragment = fragments.deq() //(p)
			if (curr_fragment.z < z_buffer(curr_fragment.y.to[I32], curr_fragment.x.to[I32])) {
				pixels.enq(Pixel(curr_fragment.x, curr_fragment.y, curr_fragment.color))
				z_buffer(curr_fragment.y.to[I32], curr_fragment.x.to[I32]) = curr_fragment.z
				pixel_reg := pixel_reg + 1.to[Int]
			}
		}

		//pixels.numel.to[Int]
		pixel_reg.value 
	}

	def rasterization2(flag			: Boolean,
					   max_min		: RegFile1[UInt8],
					   xmax_index	: Reg[Int], 
					   ymax_index	: Reg[Int], 
					   sample_triangle2D : triangle2D, 
					   fragments 	: FIFO[CandidatePixel]) : Int = {

		def pixel_in_triangle(x : Int16, y : Int16, tri2D : triangle2D) : Boolean = {

			val pi0 =  (x - tri2D.x0.to[Int16]) * (tri2D.y1.to[Int16] - tri2D.y0.to[Int16]) -
					   (y - tri2D.y0.to[Int16]) * (tri2D.x1.to[Int16] - tri2D.x0.to[Int16])
			val pi1 =  (x - tri2D.x1.to[Int16]) * (tri2D.y2.to[Int16] - tri2D.y1.to[Int16]) - 
					   (y - tri2D.y1.to[Int16]) * (tri2D.x2.to[Int16] - tri2D.x1.to[Int16])
			val pi2 =  (x - tri2D.x2.to[Int16]) * (tri2D.y0.to[Int16] - tri2D.y2.to[Int16]) -
					   (y - tri2D.y2.to[Int16]) * (tri2D.x0.to[Int16] - tri2D.x2.to[Int16])
			
			(pi0 >= 0.to[Int16] && pi1 >= 0.to[Int16] && pi2 >= 0.to[Int16])

		}

		val color = 100.to[UInt8]
		val x_max = xmax_index.value //(max_min(1).to[I32] - max_min(0).to[I32]).to[I32]
		val y_max = ymax_index.value //(max_min(3).to[I32] - max_min(2).to[I32]).to[I32]

		val frag_reg = Reg[Int](0)
		Pipe { frag_reg := 0.to[Int] }
		if ( (!flag) ) { 
		   	Foreach(x_max by 1, y_max by 1 par 1){ (x_t, y_t) =>
				val x = max_min(0).to[Int16] + x_t.to[Int16]
				val y = max_min(2).to[Int16] + y_t.to[Int16]

				val in_triangle = pixel_in_triangle(x, y, sample_triangle2D) 

				val frag_x = x.to[UInt8]
				val frag_y = y.to[UInt8]
				val frag_z = sample_triangle2D.z
				val frag_color = color 

				if (in_triangle == true) {
					fragments.enq( CandidatePixel(frag_x, frag_y, frag_z, frag_color) ) 
					frag_reg := frag_reg +  1.to[Int]  
				}
			} /*
			Foreach(max_index.value.to[I32] by 1 par 4) { k =>
			 	val x = max_min(0).to[Int] + k.to[Int] % max_min(4).to[Int]
			  	val y = max_min(2).to[Int] + k.to[Int] / max_min(4).to[Int]

			  	val in_triangle = pixel_in_triangle(x, y, sample_triangle2D) 

				val frag_x = x.to[UInt8]
				val frag_y = y.to[UInt8]
			   	val frag_z = sample_triangle2D.z
				val frag_color = color 
	     	//	if  (in_triangle == true) {
    			Pipe { fragments.enq( CandidatePixel(frag_x, frag_y, frag_z, frag_color), in_triangle == true)	}									
		    //    }
		    } */
		}
		frag_reg.value 
		//mux(flag, 0.to[Int], fragments.numel.to[Int]) /* if (flag) 0 else i */
	}

	/* calculate bounding box for 2D triangle */
	def rasterization1(sample_triangle2D : Reg[triangle2D], 
					   max_min			 : RegFile1[UInt8],
					   xmax_index		 : Reg[Int], 
					   ymax_index 		 : Reg[Int]) : Boolean = {

		def check_clockwise(tri2D : triangle2D) : Int = {
			(tri2D.x2.to[Int] - tri2D.x0.to[Int]) * (tri2D.y1.to[Int] - tri2D.y0.to[Int]) - (tri2D.y2.to[Int] - tri2D.y0.to[Int]) * (tri2D.x1.to[Int] - tri2D.x0.to[Int])
		}

		def find_min(a : UInt8, b : UInt8, c : UInt8) : UInt8 = {
			mux(a < c, mux(a < b, a , b), mux(c < b, c, b))
		}

		def find_max(a : UInt8, b : UInt8, c : UInt8) : UInt8 = {
			mux(a > c, mux(a > b, a , b), mux(c > b, c, b))
		}

		val this_check = Reg[Int](0)
		this_check := check_clockwise(sample_triangle2D.value) 

		/* correspomds to the clockwise_vertices function */
		if (this_check.value != 0.to[Int]) {

			val check_negative = this_check.value < 0.to[Int]
			val new_x0 = mux(check_negative, sample_triangle2D.x1, sample_triangle2D.x0)
			val new_y0 = mux(check_negative, sample_triangle2D.y1, sample_triangle2D.y0)
			val new_x1 = mux(check_negative, sample_triangle2D.x0, sample_triangle2D.x1)
			val new_y1 = mux(check_negative, sample_triangle2D.y0, sample_triangle2D.y1)

			sample_triangle2D := triangle2D(new_x0, new_y0, new_x1, new_y1, 
										  		   sample_triangle2D.x2, sample_triangle2D.y2,
										    	   sample_triangle2D.z) 
			
			val min_x =  find_min(sample_triangle2D.x0, sample_triangle2D.x1, sample_triangle2D.x2)
			val max_x =  find_max(sample_triangle2D.x0, sample_triangle2D.x1, sample_triangle2D.x2)
			val min_y =  find_min(sample_triangle2D.y0, sample_triangle2D.y1, sample_triangle2D.y2)
			val max_y =  find_max(sample_triangle2D.y0, sample_triangle2D.y1, sample_triangle2D.y2)

			
			max_min(0) = min_x 
			max_min(1) = max_x 
			max_min(2) = min_y 
			max_min(3) = max_y 
			ymax_index := (max_y - min_y).to[Int]  //Pipe { max_min(4) = max_x - min_x }
			xmax_index := (max_x - min_x).to[Int] 

		}

		(this_check.value == 0.to[Int])
	}

	def projection(sample_triangle3D : triangle3D, 
				   proj_triangle2D	 : Reg[triangle2D], 
				   angle			 : Int) : Unit = {

		val x0 = Reg[UInt8]
		val y0 = Reg[UInt8]

		val x1 = Reg[UInt8]
		val y1 = Reg[UInt8]

		val x2 = Reg[UInt8]
		val y2 = Reg[UInt8]

		val z = Reg[UInt8]

		x0 := mux(angle == 0, sample_triangle3D.x0, 
					mux(angle == 1, sample_triangle3D.x0, sample_triangle3D.z0))
		y0 := mux(angle == 0, sample_triangle3D.y0, 	
					mux(angle == 1, sample_triangle3D.z0, sample_triangle3D.y0))

		x1 := mux(angle == 0, sample_triangle3D.x1, 
					mux(angle == 1, sample_triangle3D.x1, sample_triangle3D.z1))
		y1 := mux(angle == 0, sample_triangle3D.y1, 	
					mux(angle == 1, sample_triangle3D.z1, sample_triangle3D.y1))

		x2 := mux(angle == 0, sample_triangle3D.x2, 
		    		 mux(angle == 1, sample_triangle3D.x2, sample_triangle3D.z2))
		y2 := mux(angle == 0, sample_triangle3D.y2, 	
					 mux(angle == 1, sample_triangle3D.z2, sample_triangle3D.y2)) 

		val f1 = sample_triangle3D.z0 / 3.to[UInt8] + sample_triangle3D.z1 / 3.to[UInt8] + sample_triangle3D.z2 / 3.to[UInt8]
		val f2 = sample_triangle3D.y0 / 3.to[UInt8] + sample_triangle3D.y1 / 3.to[UInt8] + sample_triangle3D.y2 / 3.to[UInt8]
		val f3 = sample_triangle3D.x0 / 3.to[UInt8] + sample_triangle3D.x1 / 3.to[UInt8] + sample_triangle3D.x2 / 3.to[UInt8]

		 z :=  mux(angle == 0, f1, mux(angle == 1, f2, f3)) 
		

		proj_triangle2D := triangle2D(x0.value, y0.value, x1.value, y1.value, x2.value, y2.value, z.value) 
	}

	def main(args: Array[String]): Void = {

		type T = FixPt[FALSE, _8, _0]

		val img_y_size = 256
		val img_x_size = 256

		val num_triangles = 3192 
		val last_triangle =  args(1).to[Int]

		val tri_start  = args(0).to[Int]

		val run_on_board = args(2).to[Int] > 0.to[Int]  
		val input_file_name = if (run_on_board) "/home/jcamach2/Rendering3D/input_triangles.csv" else s"$DATA/rosetta/3drendering_input_triangles.csv"
		val output_file_name = if (run_on_board) "/home/jcamach2/Rendering3D/sw_output.csv" else s"$DATA/rosetta/3drendering_sw_output.csv"
		val input_trianges_csv = loadCSV2D[T](input_file_name, ",", "\n")

		val output_image = DRAM[UInt8](img_y_size, img_x_size)
 
		/* Set all triangle3D structures in DRAM */
		val triangle3D_vector_host = Array.tabulate(num_triangles){ inx => {
																		val i = inx 
																		val x0 = input_trianges_csv.apply(i,0).to[UInt8]
																		val y0 = input_trianges_csv.apply(i,1).to[UInt8]
																		val z0 = input_trianges_csv.apply(i,2).to[UInt8]
																		val x1 = input_trianges_csv.apply(i,3).to[UInt8]
																		val y1 = input_trianges_csv.apply(i,4).to[UInt8]
																		val z1 = input_trianges_csv.apply(i,5).to[UInt8]
																		val x2 = input_trianges_csv.apply(i,6).to[UInt8]
																		val y2 = input_trianges_csv.apply(i,7).to[UInt8]
																		val z2 = input_trianges_csv.apply(i,8).to[UInt8]
																		val newTriangle = triangle3D(x0,y0,z0,x1,y1,z1,x2,y2,z2, 0.to[UInt32], 0.to[UInt16], 0.to[UInt8])
																		newTriangle
																	}
																  }
	
	

		val triangle3D_vector_dram = DRAM[triangle3D](num_triangles)
		setMem(triangle3D_vector_dram, triangle3D_vector_host)

		val host_z_buffer = DRAM[UInt8](img_y_size, img_x_size)
		val host_frame_buffer = DRAM[UInt8](img_y_size, img_x_size)

		val vec_sram_len = num_triangles

		Accel {
			val angle = 0.to[Int]
	
			val z_buffer = SRAM[UInt8](img_y_size, img_x_size)
			val frame_buffer  = SRAM[UInt8](img_y_size, img_x_size)

			/* instantiate buffer */
			Foreach(img_y_size by 1, img_x_size by 1 par 16) { (i,j) =>
				z_buffer(i,j) = 255.to[UInt8]
				frame_buffer(i,j) = 0.to[UInt8]
			}
			
			val do_triangles = last_triangle - tri_start
			Foreach(do_triangles by vec_sram_len) { i =>

				val load_len = min(vec_sram_len.to[Int], do_triangles - i)
				val triangle3D_vector_sram = SRAM[triangle3D](vec_sram_len)

				triangle3D_vector_sram load triangle3D_vector_dram(i :: i + vec_sram_len par 4) 
	
				Foreach(load_len by 1) { c =>

					val curr_triangle3D = triangle3D_vector_sram(c + tri_start)
					val tri2D = Reg[triangle2D].buffer

					val max_min	= RegFile[UInt8](5)
					val xmax_index = Reg[Int](0)
					val ymax_index = Reg[Int](0)

					val pixels = FIFO[Pixel](500)
	
					val flag = Reg[Boolean](false)
					val size_fragment = Reg[Int](0.to[Int])
					val size_pixels = Reg[Int](0.to[Int])

					val fragment = FIFO[CandidatePixel](500)

					'proj.Pipe { projection(curr_triangle3D, tri2D, angle) }
					'rast1.Pipe { flag := rasterization1(tri2D, max_min, xmax_index, ymax_index) } 
					'rast2.Pipe { size_fragment := rasterization2(flag.value, max_min, xmax_index, ymax_index, tri2D, fragment) }
					'zcull.Pipe { size_pixels := zculling(fragment, size_fragment.value, pixels, z_buffer) }
					'color.Pipe { coloringFB(size_pixels.value, pixels, frame_buffer) }

				}

			}

			Parallel { 
				//host_z_buffer store z_buffer 
				host_frame_buffer store frame_buffer
			}
		}

		//val z_buffer_screen = getMatrix(host_z_buffer)
		val frame_screen = getMatrix(host_frame_buffer)
	
		val print_frame = false
		if (print_frame) {      
			printMatrix(frame_screen)
		}

		/* output image file that's created by the Rosetta Software implementation - use for testing */
		val sw_original_output = loadCSV2D[T](output_file_name, ",", "\n")

		val print_just_in_case = false
		if (print_just_in_case) { /* this is for making sure that I'm loading the file correctly */
			for (i <- 0 until img_y_size) {
				for (j <- 0 until img_x_size) {
					print( sw_original_output.apply(i,j) )
				}
				println() 
			}
		}

		val frame_output = (0 :: img_y_size,  0 :: img_x_size) { (i,j) => if (frame_screen(i,j) > 0) 1.to[T] else 0.to[T] }
		val gold_output = (0 :: img_y_size,  0 :: img_x_size) { (i,j) => if (sw_original_output.apply(img_y_size - 1- i,j) > 0) 1.to[T] else 0.to[T] }

		val print_output = true
		if (print_output) { /* this is for making sure that I'm loading the file correctly */
			for (i <- 0 until img_y_size) {
				for (j <- 0 until img_x_size) {
					print( frame_output(img_y_size - 1 - i,j) )
				}
				println() 
			}
		}

		val cksum = frame_output.zip(gold_output) { _ == _ }.reduce( _ && _ )
		println("Pass? " + cksum)

	}
}