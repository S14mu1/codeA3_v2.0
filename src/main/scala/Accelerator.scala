import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })

  // Optimized state machine with fewer states
  val sIdle :: sRead :: sWrite :: sDone :: sCheckNeighbor :: Nil = Enum(5)
  val currentState = RegInit(sIdle)
  
  // Position and data registers
  val posX = RegInit(0.U(5.W))
  val posY = RegInit(0.U(5.W))
  val outputData = RegInit(0.U(32.W))
  val memAddr = RegInit(0.U(16.W))
  
  // Neighbor check counter (0=up, 1=down, 2=left, 3=right)
  val neighborIdx = RegInit(0.U(2.W))

  // Constants
  val gridSize = 20.U
  val maxCoord = 19.U
  val outputOffset = 400.U
  val whitePixel = 255.U
  val blackPixel = 0.U

  // Address calculation helpers
  def calcInputAddr(xPos: UInt, yPos: UInt): UInt = xPos + gridSize * yPos
  def calcOutputAddr(xPos: UInt, yPos: UInt): UInt = calcInputAddr(xPos, yPos) + outputOffset
  
  // Default outputs
  io.writeEnable := false.B
  io.dataWrite := outputData
  io.address := memAddr
  io.done := false.B

  // Boundary and pixel checks
  val atBorder = (posX === 0.U) || (posX === maxCoord) || (posY === 0.U) || (posY === maxCoord)
  val isBlack = io.dataRead === blackPixel
  val isWhite = io.dataRead === whitePixel
  
  // Next position calculation
  val nextY = Mux(posY < maxCoord, posY + 1.U, 0.U)
  val nextX = Mux(posY < maxCoord, posX, posX + 1.U)
  val processingComplete = (posX >= gridSize) || (posY >= gridSize)

  switch(currentState) {
    is(sIdle) {
      when(io.start) {
        posX := 0.U
        posY := 0.U
        memAddr := 0.U
        currentState := sRead
      }
    }
    
    is(sRead) {
      // Optimization: decide immediately based on current position/data
      when(atBorder || isBlack) {
        // Can write black immediately
        outputData := blackPixel
        memAddr := calcOutputAddr(posX, posY)
        currentState := sWrite
      }.otherwise {
        // Need to check neighbors, start with first neighbor
        neighborIdx := 0.U
        memAddr := calcInputAddr(posX, posY - 1.U) // Check up first
        currentState := sCheckNeighbor
      }
    }
    
    is(sCheckNeighbor) {
      when(isBlack) {
        // Found a black neighbor, write black and move on
        outputData := blackPixel
        memAddr := calcOutputAddr(posX, posY)
        currentState := sWrite
      }.elsewhen(neighborIdx === 3.U) {
        // Checked all 4 neighbors, all white - write white
        outputData := whitePixel
        memAddr := calcOutputAddr(posX, posY)
        currentState := sWrite
      }.otherwise {
        // Check next neighbor
        neighborIdx := neighborIdx + 1.U
        memAddr := MuxLookup(neighborIdx, 0.U)(Seq(
          0.U -> calcInputAddr(posX, posY + 1.U),     // down
          1.U -> calcInputAddr(posX - 1.U, posY),     // left
          2.U -> calcInputAddr(posX + 1.U, posY)      // right
        ))
        currentState := sCheckNeighbor
      }
    }
    
    is(sWrite) {
      io.writeEnable := true.B
      
      // Pre-calculate next position and address
      posY := nextY
      posX := nextX
      memAddr := calcInputAddr(nextX, nextY)
      
      when(nextX < gridSize && nextY < gridSize) {
        currentState := sRead
      }.otherwise {
        currentState := sDone
      }
    }
    
    is(sDone) {
      io.done := true.B
      currentState := sDone
    }
  }
}
