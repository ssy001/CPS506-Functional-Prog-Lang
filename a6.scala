// To run the program, 
// 1-compile in prompt  > scalac a6.scala
// 2-run the class file > scala a6
// NOTE: main method in (Object a6) doesn't input error check only valid commands such as "Q", "d2 S", "d2 S 7", etc.

import scala.io.StdIn

class ChessPiece(var piece:String){
   def set(P:String) = piece = P
   def get() = piece
   def show() = print(piece)

   override def toString() = this.get()
}

class ChessBoard(){
   val Board = Array.ofDim[ChessPiece](8,8)
   var BlackTaken = Array[ChessPiece]()
   var WhiteTaken = Array[ChessPiece]()
   val Blacks = Set("R", "N", "B", "Q", "K", "P")
   val ValidMoves = Map(
      "k" -> Set("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
      "q" -> Set("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
      "b" -> Set("NE", "SE", "SW", "NW"),
      "n" -> Set("NNW", "NNE", "NWW", "NEE", "SSW", "SSE", "SWW", "SEE"),
      "r" -> Set("N", "E", "S", "W"),
      "p" -> Set("SE", "S", "SW"),
      "K" -> Set("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
      "Q" -> Set("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
      "B" -> Set("NE", "SE", "SW", "NW"),
      "N" -> Set("NNW", "NNE", "NWW", "NEE", "SSW", "SSE", "SWW", "SEE"),
      "R" -> Set("N", "E", "S", "W"),
      "P" -> Set("NE", "N", "NW"))
   val SizeMoves = Map( 
      "k" -> 1, "q" -> 7, "b" -> 7, "n" -> 1, "r" -> 7, "p" -> 1,
      "K" -> 1, "Q" -> 7, "B" -> 7, "N" -> 1, "R" -> 7, "P" -> 1)

   def isValidMove(Pc:ChessPiece, Dir:String) = {
      if ( ValidMoves(Pc.get())(Dir) ) true
      else false
   }

   def init() = {
      Board(0)(0) = new ChessPiece("r")
      Board(0)(1) = new ChessPiece("n")
      Board(0)(2) = new ChessPiece("b")
      Board(0)(3) = new ChessPiece("q")
      Board(0)(4) = new ChessPiece("k")
      Board(0)(5) = new ChessPiece("b")
      Board(0)(6) = new ChessPiece("n")
      Board(0)(7) = new ChessPiece("r")
      Board(1)(0) = new ChessPiece("p")
      Board(1)(1) = new ChessPiece("p")
      Board(1)(2) = new ChessPiece("p")
      Board(1)(3) = new ChessPiece("p")
      Board(1)(4) = new ChessPiece("p")
      Board(1)(5) = new ChessPiece("p")
      Board(1)(6) = new ChessPiece("p")
      Board(1)(7) = new ChessPiece("p")
      for (rank <- 2 to 5 ) {
         for( file <- 0 to 7 ) {
            Board(rank)(file) = new ChessPiece(" ")
         }
      }
      Board(6)(0) = new ChessPiece("P")
      Board(6)(1) = new ChessPiece("P")
      Board(6)(2) = new ChessPiece("P")
      Board(6)(3) = new ChessPiece("P")
      Board(6)(4) = new ChessPiece("P")
      Board(6)(5) = new ChessPiece("P")
      Board(6)(6) = new ChessPiece("P")
      Board(6)(7) = new ChessPiece("P")
      Board(7)(0) = new ChessPiece("R")
      Board(7)(1) = new ChessPiece("N")
      Board(7)(2) = new ChessPiece("B")
      Board(7)(3) = new ChessPiece("Q")
      Board(7)(4) = new ChessPiece("K")
      Board(7)(5) = new ChessPiece("B")
      Board(7)(6) = new ChessPiece("N")
      Board(7)(7) = new ChessPiece("R")
   }

   def show() = {
      println("  ---------------")
      for( rank <- 0 to 7 ) {
         print(8-rank) 
         for (file <- 0 to 7 ) {
            if (file != 7) print( "|" + (Board(rank)(file)).get() )
            else print( "|" + (Board(rank)(file)).get() + "|" )
         }
         println()
      }
      println("  ---------------")
      println("  a b c d e f g h")
   }

   def getPosPiece(Pos:String):ChessPiece = {
      val RankIdx = 7 - (Pos.charAt(1) - 49)       // Rank 1..8
      val FileIdx = Pos.charAt(0) - 97             // File a..h
      Board(RankIdx)(FileIdx)
   }  

   def addPieceToTaken(Pc:ChessPiece) = {
      if ( Blacks(Pc.get()) ) BlackTaken = BlackTaken :+ Pc
      else                    WhiteTaken = WhiteTaken :+ Pc
//      return null
   }

   def showTaken(Tk:Array[ChessPiece]) = {
      for ( i <- 0 until Tk.length) {
         print(Tk(i).get + ", ")
      }
      println()
   }

   def posHasPiece(Pos:String) = {
      if (getPosPiece(Pos).get == " ") false
      else true
   }
   
   def validPos(Pos:String) = {
      val RankIdx = 7 - (Pos.charAt(1) - 49)       // Rank 1..8
      val FileIdx = Pos.charAt(0) - 97             // File a..h
      if ( RankIdx >= 0 && RankIdx <= 7 && FileIdx >= 0 && FileIdx <= 7 ) true
      else false      
   }   

   def nextPos(Pos:String, Dir:String):String = {
      val RankIdx = 7 - (Pos.charAt(1) - 49)       // Rank 1..8
      val FileIdx = Pos.charAt(0) - 97             // File a..h
      val (nRankIdx, nFileIdx) = this.nextPosCalc(RankIdx,FileIdx,Dir)
      //convert back to std chess positions
      val NewFile = (nFileIdx+97).toChar
      val NewRank = (8-nRankIdx+48).toChar
      "" + NewFile + NewRank
   }

   def nextPosCalc(RankIdx:Int, FileIdx:Int, Dir:String):(Int,Int) = {
      Dir match {             //Board(RankIdx)(FileIdx) -> Board(0..7)(0..7) 
         case "N" => (RankIdx-1, FileIdx)
         case "S" => (RankIdx+1, FileIdx)
         case "E" => (RankIdx, FileIdx+1)
         case "W" => (RankIdx-1, FileIdx-1)
         case "NE" => (RankIdx-1, FileIdx+1)
         case "NW" => (RankIdx-1, FileIdx-1)
         case "SE" => (RankIdx+1, FileIdx+1)
         case "SW" => (RankIdx+1, FileIdx-1)
         case "NNE" => (RankIdx-2, FileIdx+1)
         case "NNW" => (RankIdx-2, FileIdx-1)
         case "NEE" => (RankIdx-1, FileIdx+2)
         case "NWW" => (RankIdx-1, FileIdx-2)
         case "SSE" => (RankIdx+2, FileIdx+1)
         case "SSW" => (RankIdx+2, FileIdx-1)
         case "SEE" => (RankIdx+1, FileIdx+2)
         case "SWW" => (RankIdx+1, FileIdx-2)
         case _     => (0,0)
      }
   }

   def samePiece(Pc1:String, Pc2:String):Boolean = {
      if ( Pc1.charAt(0).isUpper && Pc2.charAt(0).isUpper ) true
      else if ( Pc1.charAt(0).isUpper == false && Pc2.charAt(0).isUpper == false ) true
      else false
   }

   def convToPos(rankIdx:Int,fileIdx:Int):String = {
//      val RankIdx = 7 - (Pos.charAt(1) - 49)       // Rank 1..8
//      val FileIdx = Pos.charAt(0) - 97             // File a..h
//      val (nRankIdx, nFileIdx) = this.nextPosCalc(RankIdx,FileIdx,Dir)
      //convert back to std chess positions
      val file = (fileIdx+97).toChar
      val rank = (8-rankIdx+48).toChar
      "" + file + rank
   }
   
   def convToIdx(pos:String):(Int,Int) = {
      val rankIdx = 7 - (pos.charAt(1) - 49)       // Rank 1..8
      val fileIdx = pos.charAt(0) - 97             // File a..h
      (rankIdx,fileIdx)   
   }

   def findBestPos(rankIdx:Int,fileIdx:Int,dir:String,pc:ChessPiece,movesize:Int):(Int,Int) = {
      val (nRankIdx, nFileIdx) = nextPosCalc(rankIdx, fileIdx, dir)
      val nPos = convToPos(nRankIdx, nFileIdx)
      if ( movesize >= 1 && validPos(nPos) ) {
         if ( !posHasPiece(nPos) )
            findBestPos(nRankIdx, nFileIdx, dir, pc, movesize-1)
         else  {   //nPos has a piece (not empty)
            if ( samePiece(pc.get, getPosPiece(nPos).get )) return (rankIdx,fileIdx)
            else return (nRankIdx,nFileIdx)
         }
      }
      else return (rankIdx,fileIdx)
   }

   def movePiece(pos:String,dir:String,movesize:Int) = {
      if ( posHasPiece(pos) && isValidMove( getPosPiece(pos), dir) ){
         val (posRank,posFile) = convToIdx(pos)
         val maxMoveSz = movesize.min( SizeMoves(getPosPiece(pos).get))
         val (bPosRank,bPosFile) = findBestPos(posRank, posFile, dir, getPosPiece(pos), maxMoveSz)
         val bPos = convToPos(bPosRank,bPosFile)
         if ( !bPos.equals(pos) ) {
            if ( posHasPiece(bPos) && !samePiece( getPosPiece(pos).get, getPosPiece(bPos).get ) )
               addPieceToTaken( getPosPiece(bPos) )
            Board(bPosRank)(bPosFile) = new ChessPiece( getPosPiece(pos).get )
            Board(posRank) (posFile)  = new ChessPiece( " " )
         }
      }
   }
}


object a6 {

   def main(args:Array[String]):Unit = {
      val board = new ChessBoard()
      board.init()
      board.show()
      print("Input move (Q to quit): ")
      var cInput = StdIn.readLine
      while ( !cInput.equals("Q") ) {
         var cInArgs = cInput.split(" ")
         if ( cInArgs.length == 2 )
            board.movePiece( cInArgs(0), cInArgs(1), 7)
         else 
            if ( cInArgs.length == 3 ) 
               board.movePiece( cInArgs(0), cInArgs(1), cInArgs(2).toInt )
         board.show()
         print("Black Taken: ")
         board.showTaken(board.BlackTaken)
         print("White Taken: ")
         board.showTaken(board.WhiteTaken)
         print("Input move (Q to quit): ")
         cInput = StdIn.readLine

      }
   }
}





