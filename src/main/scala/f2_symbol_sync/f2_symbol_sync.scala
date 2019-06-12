// Dsp-block f2_symbol_sync
//
// Take a digital IQ input stream and evaluate the correlation
// with the 801.11n short and long training fields.  These are
// specified in IEEE 801.11-2012, Annex L.
//
// The results of the correlation are squared and sent to an energy
// detector.
//
// This version by Gregory Wright, 11 June 2019.
//
// Inititally written by dsp-blocks initmodule.sh, 20190611
package f2_symbol_sync

import chisel3.experimental._
import chisel3._
import dsptools.{DspTester, DspTesterOptionsManager, DspTesterOptions}
import dsptools.numbers._
import breeze.math.Complex

class f2_symbol_sync_io[T <:Data](proto: T,n: Int)
   extends Bundle {
        val A       = Input(Vec(n,proto))
        val B       = Output(Vec(n,proto))
        override def cloneType = (new f2_symbol_sync_io(proto.cloneType,n)).asInstanceOf[this.type]
   }

class f2_symbol_sync[T <:Data] (proto: T,n: Int) extends Module {
    val io = IO(new f2_symbol_sync_io( proto=proto, n=n))
    val register=RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(proto.cloneType))))
    register:=io.A
    io.B:=register
}

//This gives you verilog
object f2_symbol_sync extends App {
    chisel3.Driver.execute(args, () => new f2_symbol_sync(
        proto=DspComplex(UInt(16.W),UInt(16.W)), n=8)
    )
}

//This is a simple unit tester for demonstration purposes
class unit_tester(c: f2_symbol_sync[DspComplex[UInt]] ) extends DspTester(c) {
//Tests are here
    poke(c.io.A(0).real, 5)
    poke(c.io.A(0).imag, 102)
    step(5)
    fixTolLSBs.withValue(1) {
        expect(c.io.B(0).real, 5)
        expect(c.io.B(0).imag, 102)
    }
}

//This is the test driver
object unit_test extends App {
    iotesters.Driver.execute(args, () => new f2_symbol_sync(
            proto=DspComplex(UInt(16.W),UInt(16.W)), n=8
        )
    ){
            c=>new unit_tester(c)
    }
}
