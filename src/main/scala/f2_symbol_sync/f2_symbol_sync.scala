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
//

package f2_symbol_sync

import chisel3.experimental._
import chisel3._
import chisel3.util._
import dsptools.{DspTester, DspTesterOptionsManager, DspTesterOptions}
import dsptools.numbers._
import breeze.math.Complex
import edge_detector._
import prog_delay._

class f2_symbol_sync_io[T <: DspComplex[SInt], U <: UInt, V <: Bits, W <: DspComplex[SInt]](samplesProto: T,
      			     		       	    	             controlProto: V,
                                                                     outputProto1: U,
                                                                     outputProto2: V,
								     debugProto:   W)
   extends Bundle {
        val iqSamples       = Input(samplesProto.cloneType)
	val syncSearch	    = Input(controlProto.cloneType)
	val passThru        = Input(controlProto.cloneType)
        val iqSyncedSamples = Output(debugProto.cloneType)
	val shortMod        = Output(outputProto1.cloneType)
        val syncMetric      = Output(outputProto1.cloneType)
        val syncFound       = Output(outputProto2.cloneType)
        override def cloneType = (new f2_symbol_sync_io(samplesProto.cloneType,
                                                        controlProto.cloneType,
                                                        outputProto1.cloneType,
							outputProto2.cloneType,
							debugProto.cloneType)).asInstanceOf[this.type]
   }

class f2_symbol_sync[T <: DspComplex[SInt], U <: UInt, V <: Bits, W <: DspComplex[SInt]] (
  samplesProto: T,
  controlProto: V,
  outputProto1: U,
  outputProto2: V,
  debugProto:   W,
  n:          Int = 16,
  resolution: Int = 32
) extends Module {
  val io = IO(new f2_symbol_sync_io(samplesProto = DspComplex(SInt(n.W), SInt(n.W)),
      	   	  		    controlProto = Bool(),
                                    outputProto1 = UInt(resolution.W),
   				    outputProto2 = Bool(),
				    debugProto   = DspComplex(SInt(resolution.W), SInt(resolution.W))))

  val variableDelay = Module(new prog_delay(proto = DspComplex(SInt(n.W), SInt(n.W)), maxdelay = 64))
  val edgeDetector  = Module(new edge_detector())

  val sampleType = DspComplex(SInt(n.W), SInt(n.W)).cloneType
  val inRegType  = DspComplex(FixedPoint(n.W, (n-2).BP), FixedPoint(n.W, (n-2).BP)).cloneType
  val debugFilterType = DspComplex(SInt(resolution.W), SInt(resolution.W)).cloneType

  // The Legacy Long Training Field (L-LTF), from IEEE 802.11-2012, Annex L.
  val longTrainingField =
    Seq(Complex(-0.1560,  0.0000),
        Complex( 0.0120, -0.0980),
        Complex( 0.0920, -0.1060),
        Complex(-0.0920, -0.1150),
        Complex(-0.0030, -0.0540),
        Complex( 0.0750,  0.0740),
        Complex(-0.1270,  0.0210),
        Complex(-0.1220,  0.0170),
        Complex(-0.0350,  0.1510),
        Complex(-0.0560,  0.0220),
        Complex(-0.0600, -0.0810),
        Complex( 0.0700, -0.0140),
        Complex( 0.0820, -0.0920),
        Complex(-0.1310, -0.0650),
        Complex(-0.0570, -0.0390),
        Complex( 0.0370, -0.0980))

  val longTrainingConj    = longTrainingField.map(c => c.conjugate)
  val longTrainingCoeffs  = longTrainingConj.map(c => DspComplex.wire(FixedPoint.fromDouble(c.real, n.W, (n-2).BP),
                                                                      FixedPoint.fromDouble(c.imag, n.W, (n-2).BP)))

  // The Legacy Short Training Field (L-STF), from IEEE 801.11-2012 Annex L.
  val shortTrainingField =
    Seq(Complex( 0.0460,  0.0460),
        Complex(-0.1320,  0.0020),
        Complex(-0.0130, -0.0790),
        Complex( 0.1430, -0.0130),
        Complex( 0.0920,  0.0000),
        Complex( 0.1430, -0.0130),
        Complex(-0.0130, -0.0790),
        Complex(-0.1320,  0.0020),
        Complex( 0.0460,  0.0460),
        Complex( 0.0020, -0.1320),
        Complex(-0.0790, -0.0130),
        Complex(-0.0130,  0.1430),
        Complex( 0.0000,  0.0920),
        Complex(-0.0130,  0.1430),
        Complex(-0.0790, -0.0130),
        Complex( 0.0020, -0.1320),
        Complex( 0.0460,  0.0460),
        Complex(-0.1320,  0.0020),
        Complex(-0.0130, -0.0790),
        Complex( 0.1430, -0.0130),
        Complex( 0.0920,  0.0000),
        Complex( 0.1430, -0.0130),
        Complex(-0.0130, -0.0790),
        Complex(-0.1320,  0.0020),
        Complex( 0.0460,  0.0460),
        Complex( 0.0020, -0.1320),
        Complex(-0.0790, -0.0130),
        Complex(-0.0130,  0.1430),
        Complex( 0.0000,  0.0920),
        Complex(-0.0130,  0.1430),
        Complex(-0.0790, -0.0130),
        Complex( 0.0020, -0.1320),
        Complex( 0.0460,  0.0460),
        Complex(-0.1320,  0.0020),
        Complex(-0.0130, -0.0790),
        Complex( 0.1430, -0.0130),
        Complex( 0.0920,  0.0000),
        Complex( 0.1430, -0.0130),
        Complex(-0.0130, -0.0790),
        Complex(-0.1320,  0.0020),
        Complex( 0.0460,  0.0460),
        Complex( 0.0020, -0.1320),
        Complex(-0.0790, -0.0130),
        Complex(-0.0130,  0.1430),
        Complex( 0.0000,  0.0920),
        Complex(-0.0130,  0.1430),
        Complex(-0.0790, -0.0130),
        Complex( 0.0020, -0.1320),
        Complex( 0.0460,  0.0460),
        Complex(-0.1320,  0.0020),
        Complex(-0.0130, -0.0790),
        Complex( 0.1430, -0.0130),
        Complex( 0.0920,  0.0000),
        Complex( 0.1430, -0.0130),
        Complex(-0.0130, -0.0790),
        Complex(-0.1320,  0.0020),
        Complex( 0.0460,  0.0460),
        Complex( 0.0020, -0.1320),
        Complex(-0.0790, -0.0130),
        Complex(-0.0130,  0.1430),
        Complex( 0.0000,  0.0920),
        Complex(-0.0130,  0.1430),
        Complex(-0.0790, -0.0130),
        Complex( 0.0020, -0.1320))

  val shortTrainingConj   = shortTrainingField.map(c => c.conjugate)
  val shortTrainingCoeffs = shortTrainingConj.map(c => DspComplex.wire(FixedPoint.fromDouble(c.real, n.W, (n-2).BP),
                                                                       FixedPoint.fromDouble(c.imag, n.W, (n-2).BP)))
 
  // Six sample boxcar filter for energy detection
  val energyDetectorCoeffs = Seq.fill(6)(1.U(resolution.W))

  // A four tap filter to detect peaks in the L-STF matched filter
  var shortDetectionTemplate = Seq.fill(64)(0.U(resolution.W))
  val shortDetectionCoeffs = shortDetectionTemplate.reverse.zipWithIndex.foreach({case (_, 15) => 1.U(resolution.W)
                                                                          case (_, 23) => 1.U(resolution.W)
	  				                                  case (_, 39) => 1.U(resolution.W)
				                                          case (_, 63) => 1.U(resolution.W)
					                                  case (v,  _) => v})
  // The input, a DspComplex of SInts, stored in a register
  // and cast to a DspComplex of FixedPoints.
  val inReg = RegInit(0.U.asTypeOf(inRegType))


  // Hardware starts here with the input register
  inReg := io.iqSamples.asTypeOf(inRegType)

  val filterScale = 4
  val filterRegType = DspComplex(FixedPoint(resolution.W, ((resolution/2)-filterScale).BP),
                                 FixedPoint(resolution.W, ((resolution/2)-filterScale).BP)).cloneType

  // Compute the correlation with the long training field (L-LTF)
  val longChainTaps     = longTrainingCoeffs.reverse.map(tap => inReg * tap)
  val longTrainingChain = RegInit(VecInit(Seq.fill(longChainTaps.length + 1)(0.U.asTypeOf(filterRegType))))

  for ( i <- 0 to longChainTaps.length - 1) {
            if (i == 0) {
	        longTrainingChain(i + 1) := longChainTaps(i)
            } else {
                longTrainingChain(i + 1) := longTrainingChain(i) + longChainTaps(i)
            }
  }

  val longChainOut = longTrainingChain(longChainTaps.length)

  val longModulusReg = RegInit(0.U(resolution.W))

  val longModulus = (longChainOut.real * longChainOut.real) +
                    (longChainOut.imag * longChainOut.imag)

  longModulusReg := longModulus(33,2).asUInt << 4	// Multiply by 16 to compensate for energy
                                         	        // difference in the short and long filters

  // Average the longModulus over six samples to get the energy estimate
  val longEnergyTaps  = energyDetectorCoeffs.reverse.map(tap => longModulusReg * tap)
  val longEnergyChain = RegInit(VecInit(Seq.fill(longEnergyTaps.length + 1)(0.U(resolution.W))))

  for ( i <- 0 to longEnergyTaps.length - 1) {
            if (i == 0) {
                longEnergyChain(i + 1) := longEnergyTaps(i)
            } else {
                longEnergyChain(i + 1) := longEnergyChain(i) + longEnergyTaps(i)
            }
  }

  val longEnergyOut = longEnergyChain(longEnergyTaps.length)

  // Compute the correlation with the short training field
  val shortChainTaps     = shortTrainingCoeffs.reverse.map(tap => inReg * tap)
  val shortTrainingChain = RegInit(VecInit(Seq.fill(shortChainTaps.length + 1)(0.U.asTypeOf(filterRegType))))

  for ( i <- 0 to shortChainTaps.length - 1) {
            if (i == 0) {
                shortTrainingChain(i + 1) := shortChainTaps(i)
            } else {
                shortTrainingChain(i + 1) := shortTrainingChain(i) + shortChainTaps(i)
            }
  }

  val shortChainOut = shortTrainingChain(shortChainTaps.length)
  io.iqSyncedSamples := shortChainOut.asTypeOf(debugFilterType)  // FIXME - temporary to look at short filter.

  val shortModulusReg = RegInit(0.U(resolution.W))

  val shortModulus = (shortChainOut.real * shortChainOut.real) +
                     (shortChainOut.imag * shortChainOut.imag)

  shortModulusReg := shortModulus(33,2).asUInt >> 2   // Divide by 4 to prevent overflow
  io.shortMod := shortModulusReg // FIXME - temporary to look at short modulus
  
  // Average the shortModulus over six samples to get the energy estimate
  val shortEnergyTaps  = energyDetectorCoeffs.reverse.map(tap => shortModulusReg * tap)
  val shortEnergyChain = RegInit(VecInit(Seq.fill(shortEnergyTaps.length + 1)(0.U(resolution.W))))

  for ( i <- 0 to shortEnergyTaps.length - 1) {
            if (i == 0) {
                shortEnergyChain(i + 1) := shortEnergyTaps(i)
            } else {
                shortEnergyChain(i + 1) := shortEnergyChain(i) + shortEnergyTaps(i)
            }
  }

  val shortEnergyOut = shortEnergyChain(shortEnergyTaps.length)

  // Run the detection filter given on p.206 of A. Sibille, C. Oestges and A. Zanello,
  // MIMO: From Theory to Implementation, Burlington, MA: Academic Press, 2011.
  val shortDetectionTaps  = energyDetectorCoeffs.reverse.map(tap => shortEnergyOut * tap)
  val shortDetectionChain = RegInit(VecInit(Seq.fill(shortDetectionTaps.length + 1)(0.U(resolution.W))))

  for ( i <- 0 to shortDetectionTaps.length - 1) {
            if (i == 0) {
                shortDetectionChain(i + 1) := shortDetectionTaps(i)
            } else {
                shortDetectionChain(i + 1) := shortDetectionChain(i) + shortDetectionTaps(i)
            }
  }

  // The output of the short training detection chain is divided by 4 to normalize
  // its energy
  val shortDetectionOut = shortDetectionChain(shortDetectionTaps.length) //>> 2 FIXME dont' shift

  val detectionReg = RegInit(0.U(resolution.W))
  val relativeDelay = 36  // relative delay of the filtered
                          // short versus long energy signals.

  detectionReg  := ShiftRegister(shortDetectionOut, relativeDelay) + longEnergyOut
  io.syncMetric := detectionReg

  //
  // At this point we have the syncMetric.  Now, it is necessary to detect the magnitude of the peak
  // in a window of 16 samples.  The peak value is saved and compared to the last peak.  If the
  // current peak is less than 85 percent as high as the previous, sync is found and  I set
  // the syncFound signal.
  //

  val syncFlag        = RegInit(false.B)
  val syncFlagInhibit = RegInit(true.B)

  val windowLength = 16
  val windowCounterLength = log2Ceil(windowLength)
 
  val previousPeakVal = RegInit(0.U(resolution.W))
  val currentPeakVal  = RegInit(0.U(resolution.W))

  val windowCounter     = RegInit(0.U(windowCounterLength.W))
  val previousPeakIndex = RegInit(0.U(windowCounterLength.W))
  val currentPeakIndex  = RegInit(0.U(windowCounterLength.W))

  val peakFactorType = FixedPoint(resolution.W, (resolution-2).BP).cloneType
  val peakFactor = FixedPoint.fromDouble(0.85, resolution.W, (resolution-2).BP)

  val detectedPeakIndex = RegInit(0.U(windowCounterLength.W))

  //
  // Control interface
  //
  // The module initializes into the "inhibited" state, with the
  // matched filters running but never emitting a "foundSync" pulse.
  // When the "io.syncSearch" signal is asserted, it is synchronized
  // with the clock and "syncFlagInhibit"is set to false.
  //
  // Asserting the "io.passThru" signal overrides io.syncSearch.  It also
  // sets the variable delay that adjusts the output data alignment
  // to zero.  The variable delay remains zero as long as passThru
  // is asserted.
  //
  val passThruReg = RegInit(false.B)

  edgeDetector.io.A := io.syncSearch
  passThruReg       := io.passThru
  when (edgeDetector.io.rising && (! passThruReg)) {
       syncFlagInhibit := false.B
  } .elsewhen (passThruReg) {
       syncFlagInhibit := true.B
  }

  // state machine goes here

  when (windowCounter === 0.U) {
      previousPeakVal   := currentPeakVal
      previousPeakIndex := currentPeakIndex

      when ((! syncFlagInhibit) &&
            (peakFactor * previousPeakVal.asTypeOf(peakFactorType) > currentPeakVal.asTypeOf(peakFactorType))) {
    	   syncFlag          := true.B
           syncFlagInhibit   := true.B
	   detectedPeakIndex := previousPeakIndex
       }

      currentPeakVal    := detectionReg
      currentPeakIndex  := 0.U
      windowCounter     := windowCounter + 1.U

  } .otherwise {
      syncFlag := false.B
      when (detectionReg > currentPeakVal) {
      	   currentPeakVal   := detectionReg
      	   currentPeakIndex := windowCounter
    	   }
      windowCounter := windowCounter + 1.U
  }
  
  // Here the syncFound pulse is aligned with the output IQ samples.
  //
  // The val 'windowPadClocks' delays both the output samples and
  // the syncFound pulse, so that the peak index in the sliding
  // detection window will not change while any part of the L-LTF
  // is passing through the variable delay.
  //
  val windowPadClocks = windowLength
  io.syncFound := ShiftRegister(syncFlag, windowPadClocks)
  
  // The delay is made two parts, a constant delay implemented
  // by a shift register, and a variable delay implemented by a
  // prog_delay module.
  //
  // The desired delay is 56 clocks minus the previous peak
  // index, plus the windowPad value. The prog_delay block
  // has a 3 clock overhead.
  //
  // The prog_delay block has a 4 bit input, giving a delay of
  // 0 to 15 clocks (plus overhead).
  //
  //
  val desiredDelayClocks          = 56
  val variableDelayOverheadClocks = 3
  val maximumVariableDelayClocks  = 15

  val constantDelayClocks = desiredDelayClocks + windowPadClocks - variableDelayOverheadClocks - maximumVariableDelayClocks

  variableDelay.io.iptr_A := ShiftRegister(inReg.asTypeOf(sampleType), constantDelayClocks)
  when (io.passThru) {
       variableDelay.io.select := 0.U
  } .otherwise {
    	variableDelay.io.select := maximumVariableDelayClocks - detectedPeakIndex
  }
  //io.iqSyncedSamples      := variableDelay.io.optr_Z
}


//This gives you verilog
object f2_symbol_sync extends App {
    chisel3.Driver.execute(args, () => new f2_symbol_sync(
        samplesProto = DspComplex(SInt(16.W), SInt(16.W)),
	controlProto = Bool(),
	outputProto1 = UInt(32.W),
	outputProto2 = Bool(),
        debugProto   = DspComplex(SInt(32.W), SInt(32.W)))
    )
}

//This is a simple unit tester for demonstration purposes
class unit_tester(c: f2_symbol_sync[DspComplex[SInt], UInt, Bool, DspComplex[SInt]] ) extends DspTester(c) {
//Tests are here
    poke(c.io.iqSamples.real, 5)
    poke(c.io.iqSamples.imag, 102)
    step(5)
    expect(c.io.syncFound, false)
}

//This is the test driver
object unit_test extends App {
    iotesters.Driver.execute(args, () => new f2_symbol_sync(
            samplesProto = DspComplex(SInt(16.W), SInt(16.W)),
	    controlProto = Bool(),
	    outputProto1 = UInt(32.W),
	    outputProto2 = Bool(),
	    debugProto   = DspComplex(SInt(32.W), SInt(32.W))
        )
    ){
            c=>new unit_tester(c)
    }
}
