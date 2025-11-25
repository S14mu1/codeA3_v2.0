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

  // State machine states with descriptive names
  val sIdle :: sFetch :: sStore :: sComplete :: sNorth :: sSouth :: sWest :: sEast :: Nil = Enum(8)
  val currentState = RegInit(sIdle)
  
  // Position tracking registers
  val posX = RegInit(0.U(5.W))
  val posY = RegInit(0.U(5.W))
  val outputData = RegInit(0.U(32.W))
  val memAddr = RegInit(0.U(16.W))

  // Constants for readability
  val gridSize = 20.U
  val maxCoord = 19.U
  val outputOffset = 400.U
  val whitePixel = 255.U
  val blackPixel = 0.U

  // Helper functions for address calculation
  def calcInputAddr(xPos: UInt, yPos: UInt): UInt = xPos + gridSize * yPos
  def calcOutputAddr(xPos: UInt, yPos: UInt): UInt = calcInputAddr(xPos, yPos) + outputOffset
  
  // Default output assignments
  io.writeEnable := false.B
  io.dataWrite := outputData
  io.address := memAddr
  io.done := false.B

  // Check if position is at boundary
  val atBorder = (posX === 0.U) || (posX === maxCoord) || (posY === 0.U) || (posY === maxCoord)
  val pixelIsBlack = io.dataRead === blackPixel
  val pixelIsWhite = io.dataRead === whitePixel

  switch(currentState) {
    is(sIdle) {
      when(io.start) {
        posX := 0.U
        posY := 0.U
        memAddr := 0.U
        currentState := sFetch
      }
    }
    
    is(sFetch) {
      when(atBorder || pixelIsBlack) {
        outputData := blackPixel
        memAddr := calcOutputAddr(posX, posY)
        currentState := sStore
      }.otherwise {
        memAddr := calcInputAddr(posX, posY - 1.U)
        currentState := sNorth
      }
    }
    
    is(sStore) {
      io.writeEnable := true.B
      
      // Update position for next iteration
      when(posY < maxCoord) {
        posY := posY + 1.U
      }.otherwise {
        posX := posX + 1.U
        posY := 0.U
      }
      
      memAddr := calcInputAddr(posX, posY)
      
      when(posX < gridSize && posY < gridSize) {
        currentState := sFetch
      }.otherwise {
        currentState := sComplete
      }
    }
    
    is(sComplete) {
      io.done := true.B
      currentState := sComplete
    }
    
    is(sNorth) {
      when(pixelIsWhite) {
        memAddr := calcInputAddr(posX, posY + 1.U)
        currentState := sSouth
      }.otherwise {
        outputData := blackPixel
        memAddr := calcOutputAddr(posX, posY)
        currentState := sStore
      }
    }
    
    is(sSouth) {
      when(pixelIsWhite) {
        memAddr := calcInputAddr(posX - 1.U, posY)
        currentState := sWest
      }.otherwise {
        outputData := blackPixel
        memAddr := calcOutputAddr(posX, posY)
        currentState := sStore
      }
    }
    
    is(sWest) {
      when(pixelIsWhite) {
        memAddr := calcInputAddr(posX + 1.U, posY)
        currentState := sEast
      }.otherwise {
        outputData := blackPixel
        memAddr := calcOutputAddr(posX, posY)
        currentState := sStore
      }
    }
    
    is(sEast) {
      memAddr := calcOutputAddr(posX, posY)
      outputData := Mux(pixelIsWhite, whitePixel, blackPixel)
      currentState := sStore
    }
  }
}
