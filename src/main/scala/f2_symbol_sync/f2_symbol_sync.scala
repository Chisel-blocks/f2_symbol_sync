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

class f2_symbol_sync_io[T <: DspComplex[SInt], U <: SInt](inputProto: T, outputProto: U)
   extends Bundle {
        val iqSamples   = Input(inputProto.cloneType)
        val longEnergy  = Output(outputProto.cloneType)
        val shortEnergy = Output(outputProto.cloneType)
        override def cloneType = (new f2_symbol_sync_io(inputProto.cloneType, outputProto.cloneType)).asInstanceOf[this.type]
   }

class f2_symbol_sync[T <: DspComplex[SInt], U <: SInt] (
  inputProto:  T,
  outputProto: U,
  n:          Int = 16,
  resolution: Int = 32
) extends Module {
  val io = IO(new f2_symbol_sync_io(inputProto  = DspComplex(SInt(n.W), SInt(n.W)),
                                    outputProto = SInt(resolution.W)))

  val longTrainingCoeffs =
    Seq(DspComplex(-16488.S(n.W),      0.S(n.W)),
        DspComplex(  2536.S(n.W),  20716.S(n.W)),
        DspComplex( 19448.S(n.W),  22407.S(n.W)),
        DspComplex(-19448.S(n.W),  24310.S(n.W)),
        DspComplex(  -634.S(n.W),  11415.S(n.W)),
        DspComplex( 15854.S(n.W), -15643.S(n.W)),
        DspComplex(-26847.S(n.W),  -4439.S(n.W)),
        DspComplex(-25790.S(n.W),  -3593.S(n.W)),
        DspComplex( -7398.S(n.W), -31920.S(n.W)),
        DspComplex(-11838.S(n.W),  -4650.S(n.W)),
        DspComplex(-12683.S(n.W),  17123.S(n.W)),
        DspComplex( 14797.S(n.W),   2959.S(n.W)),
        DspComplex( 17334.S(n.W),  19448.S(n.W)),
        DspComplex(-27692.S(n.W),  13740.S(n.W)),
        DspComplex(-12049.S(n.W),   8244.S(n.W)),
        DspComplex(  7821.S(n.W),  20716.S(n.W)))


  val shortTrainingCoeffs =
    Seq(DspComplex(   456.S(n.W),  30122.S(n.W)),
        DspComplex(-18027.S(n.W),   2966.S(n.W)),
        DspComplex( -2966.S(n.W), -32632.S(n.W)),
        DspComplex(     0.S(n.W), -20994.S(n.W)),
        DspComplex( -2966.S(n.W), -32632.S(n.W)),
        DspComplex(-18027.S(n.W),   2966.S(n.W)),
        DspComplex(   456.S(n.W),  30122.S(n.W)),
        DspComplex( 10497.S(n.W), -10497.S(n.W)),
        DspComplex(-30122.S(n.W),   -456.S(n.W)),
        DspComplex( -2966.S(n.W),  18027.S(n.W)),
        DspComplex( 32632.S(n.W),   2966.S(n.W)),
        DspComplex( 20994.S(n.W),      0.S(n.W)),
        DspComplex( 32632.S(n.W),   2966.S(n.W)),
        DspComplex( -2966.S(n.W),  18027.S(n.W)),
        DspComplex(-30122.S(n.W),   -456.S(n.W)),
        DspComplex( 10497.S(n.W), -10497.S(n.W)),
        DspComplex(   456.S(n.W),  30122.S(n.W)),
        DspComplex(-18027.S(n.W),   2966.S(n.W)),
        DspComplex( -2966.S(n.W), -32632.S(n.W)),
        DspComplex(     0.S(n.W), -20994.S(n.W)),
        DspComplex( -2966.S(n.W), -32632.S(n.W)),
        DspComplex(-18027.S(n.W),   2966.S(n.W)),
        DspComplex(   456.S(n.W),  30122.S(n.W)),
        DspComplex( 10497.S(n.W), -10497.S(n.W)),
        DspComplex(-30122.S(n.W),   -456.S(n.W)),
        DspComplex( -2966.S(n.W),  18027.S(n.W)),
        DspComplex( 32632.S(n.W),   2966.S(n.W)),
        DspComplex( 20994.S(n.W),      0.S(n.W)),
        DspComplex( 32632.S(n.W),   2966.S(n.W)),
        DspComplex( -2966.S(n.W),  18027.S(n.W)),
        DspComplex(-30122.S(n.W),   -456.S(n.W)),
        DspComplex( 10497.S(n.W), -10497.S(n.W)),
        DspComplex(   456.S(n.W),  30122.S(n.W)),
        DspComplex(-18027.S(n.W),   2966.S(n.W)),
        DspComplex( -2966.S(n.W), -32632.S(n.W)),
        DspComplex(     0.S(n.W), -20994.S(n.W)),
        DspComplex( -2966.S(n.W), -32632.S(n.W)),
        DspComplex(-18027.S(n.W),   2966.S(n.W)),
        DspComplex(   456.S(n.W),  30122.S(n.W)),
        DspComplex( 10497.S(n.W), -10497.S(n.W)),
        DspComplex(-30122.S(n.W),   -456.S(n.W)),
        DspComplex( -2966.S(n.W),  18027.S(n.W)),
        DspComplex( 32632.S(n.W),   2966.S(n.W)),
        DspComplex( 20994.S(n.W),      0.S(n.W)),
        DspComplex( 32632.S(n.W),   2966.S(n.W)),
        DspComplex( -2966.S(n.W),  18027.S(n.W)),
        DspComplex(-30122.S(n.W),   -456.S(n.W)),
        DspComplex( 10497.S(n.W), -10497.S(n.W)),
        DspComplex(   456.S(n.W),  30122.S(n.W)),
        DspComplex(-18027.S(n.W),   2966.S(n.W)),
        DspComplex( -2966.S(n.W), -32632.S(n.W)),
        DspComplex(     0.S(n.W), -20994.S(n.W)),
        DspComplex( -2966.S(n.W), -32632.S(n.W)),
        DspComplex(-18027.S(n.W),   2966.S(n.W)),
        DspComplex(   456.S(n.W),  30122.S(n.W)),
        DspComplex( 10497.S(n.W), -10497.S(n.W)),
        DspComplex(-30122.S(n.W),   -456.S(n.W)),
        DspComplex( -2966.S(n.W),  18027.S(n.W)),
        DspComplex( 32632.S(n.W),   2966.S(n.W)),
        DspComplex( 20994.S(n.W),      0.S(n.W)),
        DspComplex( 32632.S(n.W),   2966.S(n.W)),
        DspComplex( -2966.S(n.W),  18027.S(n.W)),
        DspComplex(-30122.S(n.W),    -456.S(n.W)),
        DspComplex(  5248.S(n.W),  -5248.S(n.W)))

  val inReg = RegInit(DspComplex.wire(0.S(n.W), 0.S(n.W)))

  inReg := io.iqSamples

  // Compute the correlation with the long training field (LTF)
  val longChainTaps     = longTrainingCoeffs.reverse.map(tap => inReg * tap)
  val longTrainingChain = RegInit(VecInit(Seq.fill(longChainTaps.length + 1)(DspComplex.wire(0.S(resolution.W), 0.S(resolution.W)))))

  for ( i <- 0 to longChainTaps.length - 1) {
            if (i == 0) {
                longTrainingChain(i + 1) := longChainTaps(i)
            } else {
                longTrainingChain(i + 1) := longTrainingChain(i) + longChainTaps(i)
            }
  }

  val longChainOut = longTrainingChain(longChainTaps.length)

  val longOutReg = RegInit(0.S(resolution.W))

  val longModulus = (longChainOut.real * longChainOut.real + longChainOut.imag * longChainOut.imag)

  longOutReg := longModulus
  io.longEnergy := longOutReg

  // COmpute the correlation with the short training field
  val shortChainTaps     = shortTrainingCoeffs.reverse.map(tap => inReg * tap)
  val shortTrainingChain = RegInit(VecInit(Seq.fill(shortChainTaps.length + 1)(DspComplex.wire(0.S(resolution.W), 0.S(resolution.W)))))

  for ( i <- 0 to shortChainTaps.length - 1) {
            if (i == 0) {
                shortTrainingChain(i + 1) := shortChainTaps(i)
            } else {
                shortTrainingChain(i + 1) := shortTrainingChain(i) + shortChainTaps(i)
            }
  }

  val shortChainOut = shortTrainingChain(shortChainTaps.length)

  val shortOutReg = RegInit(0.S(resolution.W))

  val shortModulus = (shortChainOut.real * shortChainOut.real + shortChainOut.imag * shortChainOut.imag)

  shortOutReg := shortModulus

  io.shortEnergy := shortOutReg
}

//This gives you verilog
object f2_symbol_sync extends App {
    chisel3.Driver.execute(args, () => new f2_symbol_sync(
        inputProto = DspComplex(SInt(16.W), SInt(16.W)), outputProto = SInt(32.W))
    )
}

//This is a simple unit tester for demonstration purposes
class unit_tester(c: f2_symbol_sync[DspComplex[SInt], SInt] ) extends DspTester(c) {
//Tests are here
    poke(c.io.iqSamples.real, 5)
    poke(c.io.iqSamples.imag, 102)
    step(5)
    fixTolLSBs.withValue(1) {
        expect(c.io.longEnergy, 5)
    }
}

//This is the test driver
object unit_test extends App {
    iotesters.Driver.execute(args, () => new f2_symbol_sync(
            inputProto = DspComplex(SInt(16.W), SInt(16.W)), outputProto = SInt(32.W)
        )
    ){
            c=>new unit_tester(c)
    }
}
