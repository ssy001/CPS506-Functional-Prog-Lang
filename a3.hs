-- Notes on Usage:
-- 1) load "a3.hs" on ghci
-- 2) run "main" function (a3-moves.txt must be in the same folder as a3.hs)
--    "processMoves" function performs the following: 
--    1-reads in lines from "a3-moves.txt" 
--    2-shows the updated chess board after each move
--    3-shows the list of White and Black pieces captured
--    hit "Enter" key to advance to the next move
--    "main" function will terminate when EOF is reached. 


module Assign3 where

   import qualified Data.Map as Map
   import qualified Data.Set as Set
   import qualified Data.List as List
   import qualified Data.Char as Char
   import qualified Data.Maybe as Maybe
   import Control.Monad(when)
	
   import System.IO
   
--	chess board layout:
--	| A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8 |
--	| A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7 |
--	| A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6 |
--	| A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5 |
--	| A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4 |
--	| A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3 |
--	| A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2 |
--	| A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1 |
			
   type Position = (Char,Int)

   data Color = 
      Black | White | NC deriving (Show, Read, Ord, Eq, Bounded, Enum)

   data Standing = 
      King | Queen | Bishop | Knight | Rook | Pawn | NS deriving (Show, Read, Ord, Eq, Bounded, Enum)

   -- each chess piece is a tuple (color, standing)
   type Piece = (Color, Standing)
	
   -- each chess square can contain a piece (or can be empty)
   type Square = (Position, Piece)
	
   -- chess board is a map of 64 (k, v) = (Position, Piece)
   type Board = Map.Map Position Piece

   chessBoard' :: Board 
   chessBoard' = Map.fromList 
      [(('a',8),(Black,Rook)),(('b',8),(Black,Knight)),(('c',8),(Black,Bishop)),(('d',8),(Black,Queen))
      ,(('e',8),(Black,King)),(('f',8),(Black,Bishop)),(('g',8),(Black,Knight)),(('h',8),(Black,Rook))
      ,(('a',7),(Black,Pawn)),(('b',7),(Black,Pawn)),(('c',7),(Black,Pawn)),(('d',7),(Black,Pawn))
      ,(('e',7),(Black,Pawn)),(('f',7),(Black,Pawn)),(('g',7),(Black,Pawn)),(('h',7),(Black,Pawn))
      ,(('a',6),(NC,NS)),(('b',6),(NC,NS)),(('c',6),(NC,NS)),(('d',6),(NC,NS))
      ,(('e',6),(NC,NS)),(('f',6),(NC,NS)),(('g',6),(NC,NS)),(('h',6),(NC,NS))
      ,(('a',5),(NC,NS)),(('b',5),(NC,NS)),(('c',5),(NC,NS)),(('d',5),(NC,NS))
      ,(('e',5),(NC,NS)),(('f',5),(NC,NS)),(('g',5),(NC,NS)),(('h',5),(NC,NS))
      ,(('a',4),(NC,NS)),(('b',4),(NC,NS)),(('c',4),(NC,NS)),(('d',4),(NC,NS))
      ,(('e',4),(NC,NS)),(('f',4),(NC,NS)),(('g',4),(NC,NS)),(('h',4),(NC,NS))
      ,(('a',3),(NC,NS)),(('b',3),(NC,NS)),(('c',3),(NC,NS)),(('d',3),(NC,NS))
      ,(('e',3),(NC,NS)),(('f',3),(NC,NS)),(('g',3),(NC,NS)),(('h',3),(NC,NS))
      ,(('a',2),(White,Pawn)),(('b',2),(White,Pawn)),(('c',2),(White,Pawn)),(('d',2),(White,Pawn))
      ,(('e',2),(White,Pawn)),(('f',2),(White,Pawn)),(('g',2),(White,Pawn)),(('h',2),(White,Pawn))
      ,(('a',1),(White,Rook)),(('b',1),(White,Knight)),(('c',1),(White,Bishop)),(('d',1),(White,Queen))
      ,(('e',1),(White,King)),(('f',1),(White,Bishop)),(('g',1),(White,Knight)),(('h',1),(White,Rook))]


   chessBoard :: String
   chessBoard = unlines ["rnbqkbnr","pppppppp","........","........","........","........","PPPPPPPP","RNBQKBNR"]
   
   -- list of pieces removed (eaten) from the board
   eatenList :: [Piece]
   eatenList = []
   
------------------------------------ displaying/showing ----------------------------------
   getBoardPos :: Position -> Board -> Piece
   getBoardPos pos board =  Maybe.fromJust (Map.lookup pos board )

   showBoard :: Board -> IO [()]
   showBoard board = sequence (map print (convertBoardToLines board))

   convertBoardToLines :: Board -> [String]
   convertBoardToLines board = List.lines $ List.concat ( map (displaySquare board) [(file,rank) | rank <- [8,7..1], file <- ['a'..'h']] )
   
   displaySquare :: Board -> Position -> String
   displaySquare board (file,rank) 
      | file == 'h' = "|" ++ [showPc] ++ "|\n"
      | otherwise   = "|" ++ [showPc]
         where showPc = showPiece (getBoardPos (file,rank) board)
	
   readBoard :: String -> [((Char,Int),Piece)]
   readBoard boardStr = zipWith (\a b -> (a,b)) [(file,rank) | rank <- [8,7..1], file <- ['a'..'h']] convChessBoard2
      where convChessBoard2 = map readPiece (List.filter (\x -> x /= '\n') boardStr)
   
   test boardStr = map readPiece (List.filter (\x -> x /= '\n') boardStr)
   
   showPiece :: Piece -> Char
   showPiece (NC,NS)        = '.'
   showPiece (Black,King)   = 'k'
   showPiece (Black,Queen)  = 'q'
   showPiece (Black,Bishop) = 'b'
   showPiece (Black,Knight) = 'n'
   showPiece (Black,Rook)   = 'r'
   showPiece (Black,Pawn)   = 'p'
   showPiece (White,King)   = 'K'
   showPiece (White,Queen)  = 'Q'
   showPiece (White,Bishop) = 'B'
   showPiece (White,Knight) = 'N'
   showPiece (White,Rook)   = 'R'
   showPiece (White,Pawn)   = 'P'

   readPiece :: Char -> Piece
   readPiece '.' = (NC,NS)
   readPiece 'k' = (Black,King)
   readPiece 'q' = (Black,Queen)
   readPiece 'b' = (Black,Bishop)
   readPiece 'n' = (Black,Knight)
   readPiece 'r' = (Black,Rook)
   readPiece 'p' = (Black,Pawn)
   readPiece 'K' = (White,King)
   readPiece 'Q' = (White,Queen)
   readPiece 'B' = (White,Bishop)
   readPiece 'N' = (White,Knight)
   readPiece 'R' = (White,Rook)
   readPiece 'P' = (White,Pawn)

   getPiece :: Position -> [(Position,Piece)] -> Piece
   getPiece pos [] = (Black,Pawn)
   getPiece pos ((po,pc):rest) = if pos == po then pc else getPiece pos rest
   
------------------------------------ input (moves) parsing ----------------------------------
   -- parseInput - takes an input string "d3 NW 5", and breaks it down into ('d',3) and "NW" and "5" (optional)
   parseInputPosition :: String -> Position
   parseInputPosition input = let h = input !! 0
                                  t = input !! 1
                                  tInt = Char.digitToInt t
                              in (h,tInt)

   parseInputDirection :: String -> String
   parseInputDirection input = let rest = tail $ dropWhile (/= ' ') input
                               in if ' ' `elem` rest 
                                  then fst $ span (/=' ') rest
                                  else rest
                                  
   parseInputSize :: String -> String
   parseInputSize input = let rest = tail $ dropWhile (/=' ') input
                          in if ' ' `elem` rest
                             then dropWhile (==' ') $ snd $ span (/=' ') rest
                             else ""
   
   
   
------------------------------------ checking and executing moves ----------------------------------
-- validMove - checks if piece can move that direction - according to chess rules
--             i.e. knight can move in L, otherwise not valid
   validMove :: Piece -> String -> Bool
   validMove (col,sta) dir = case sta of King   -> elem dir ["N","NE","E","SE","S","SW","W","NW"] 
                                         Queen  -> elem dir ["N","NE","E","SE","S","SW","W","NW"] 
                                         Bishop -> elem dir ["NE","SE","SW","NW"] 
                                         Knight -> elem dir ["NNW","NNE","NWW","NEE","SSW","SSE","SWW","SEE","EEN","EES","ENN","ESS","WWN","WWS","WNN","WSS"] 
                                         Rook   -> elem dir ["N","E","S","W"] 
                                         Pawn   -> case col of Black -> elem dir ["SE","S","SW"] 
                                                               White -> elem dir ["N","NE","NW"] 
                                          
   moveDist :: Piece -> Int
   moveDist (col,sta) = case sta of King   -> 1
                                    Queen  -> 8
                                    Bishop -> 8
                                    Knight -> 1
                                    Rook   -> 8
                                    Pawn   -> 1

-------------------- Main fcn to call to testing --------------------                                   
-- makeMove - ACTIONS: 1)checks if there is a piece in the current position,
--                        a)if there is, 
--                          then checks if move is valid for the piece
--                                      then calls move to return new board
--                        b)else return board
   makeMove :: Board -> Position -> String -> String -> [Piece] -> (Board, [Piece])
   makeMove board pos dir sizeStr eatList = if (piece /= (NC,NS)) && (validMove piece dir)
                                then move board pos positions eatList 
                                else (board, eatList)
                                   where piece = getBoardPos pos board
                                         sizeInt = getSize sizeStr
                                         minSize = minimum [(moveDist piece), sizeInt]
                                         positions = take minSize (canMove pos dir)
                                         
   getSize :: String -> Int
   getSize sizeStr = if length sizeStr > 0 
                       then if Char.isDigit (sizeStr !! 0)
                            then Char.digitToInt (sizeStr !! 0)
                            else 0
                       else 9
 
-- move - takes the resulting positions list of canMove and (recursively) moves the piece in the initial position
--      - ASSUMPTIONS: 1)there is a piece in initial location, and 2)the move is valid for it
--      - ACTIONS: 1)if there is a piece in new location and color is opposite, then updates the "pieces eaten" list, and 
--                 2)recursively calls itself if either a)moves list not empty, or b)a piece is not eaten
   move :: Board -> Position -> [Position] -> [Piece] -> (Board, [Piece])
   move board _ [] list = (board, list) 
   move board pos ((f,r):xs) eatList
      | (adjPiece == (NC,NS))                       = move newBoard (f,r) xs eatList            -- if newLoc is empty
      | (adjPiece /= (NC,NS)) && (adjCol /= curCol) = move newBoard (f,r) [] (adjPiece:eatList)   -- if newLoc contains opposing piece
      | (adjPiece /= (NC,NS)) && (adjCol == curCol) = move board     pos  [] eatList            -- if newLoc contains same-side piece
      where adjPiece = getBoardPos (f,r) board 
            adjCol = getPieceCol adjPiece
            curPiece = getBoardPos pos board
            curCol = getPieceCol curPiece
            newBoard = Map.insert pos (NC,NS) (Map.insert (f,r) curPiece board)
   
   getPieceCol :: Piece -> Color
   getPieceCol (col,sta) = col
   
-- canMove   - provides a list of positions that a piece can move as allowed by the board's boundaries
   canMove :: Position -> String -> [Position]
   canMove (file,rank) dir = case dir of "NW"  -> if (file /= 'a' && rank /= 8) then (newfile,newrank):canMove (newfile,newrank) dir else []
                                                     where newfile = Char.chr $ (Char.ord file) - 1
                                                           newrank = rank + 1
                                         "NE"  -> if (file /= 'h' && rank /= 8) then (newfile,newrank):canMove (newfile,newrank) dir else []
                                                     where newfile = Char.chr $ (Char.ord file) + 1
                                                           newrank = rank + 1
                                         "SW"  -> if (file /= 'a' && rank /= 1) then (newfile,newrank):canMove (newfile,newrank) dir else []
                                                     where newfile = Char.chr $ (Char.ord file) - 1
                                                           newrank = rank - 1
                                         "SE"  -> if (file /= 'h' && rank /= 1) then (newfile,newrank):canMove (newfile,newrank) dir else []
                                                     where newfile = Char.chr $ (Char.ord file) + 1
                                                           newrank = rank - 1
                                         "N"   -> if (rank /= 8) then (file,newrank):canMove (file,newrank) dir else []
                                                     where newrank = rank + 1 
                                         "S"   -> if (rank /= 1) then (file,newrank):canMove (file,newrank) dir else []
                                                     where newrank = rank - 1 
                                         "E"   -> if (file /= 'h') then (newfile,rank):canMove (newfile,rank) dir else []
                                                     where newfile = Char.chr $ (Char.ord file) + 1
                                         "W"   -> if (file /= 'a') then (newfile,rank):canMove (newfile,rank) dir else []
                                                     where newfile = Char.chr $ (Char.ord file) - 1
                                         "NNW" -> if (file >= 'b' && rank <= 6) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) - 1
                                                           newrank = rank + 2
                                         "NNE" -> if (file <= 'g' && rank <= 6) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) + 1
                                                           newrank = rank + 2
                                         "NWW" -> if (file >= 'c' && rank <= 7) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) - 2
                                                           newrank = rank + 1
                                         "NEE" -> if (file <= 'f' && rank <= 7) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) + 2
                                                           newrank = rank + 1
                                         "SSW" -> if (file >= 'b' && rank >= 3) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) - 1
                                                           newrank = rank - 2
                                         "SSE" -> if (file <= 'g' && rank >= 3) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) + 1
                                                           newrank = rank - 2
                                         "SWW" -> if (file >= 'c' && rank >= 2) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) - 2
                                                           newrank = rank - 1
                                         "SEE" -> if (file <= 'f' && rank >= 2) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) + 2
                                                           newrank = rank - 1
                                         "EEN" -> if (file <= 'f' && rank <= 7) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) + 2
                                                           newrank = rank + 1
                                         "EES" -> if (file <= 'f' && rank >= 2) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) + 2
                                                           newrank = rank - 1
                                         "ENN" -> if (file <= 'g' && rank <= 6) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) + 1
                                                           newrank = rank + 2
                                         "ESS" -> if (file <= 'g' && rank >= 3) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) + 1
                                                           newrank = rank - 2
                                         "WWN" -> if (file >= 'c' && rank <= 7) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) - 2
                                                           newrank = rank + 1
                                         "WWS" -> if (file >= 'c' && rank >= 2) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) - 2
                                                           newrank = rank - 1
                                         "WNN" -> if (file >= 'b' && rank <= 6) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) - 1
                                                           newrank = rank + 2
                                         "WSS" -> if (file >= 'b' && rank >= 3) then [(newfile,newrank)] else []
                                                     where newfile = Char.chr $ (Char.ord file) - 1
                                                           newrank = rank - 2
                                 
   getWhites :: [Piece] -> [Piece]
   getWhites [] = []
   getWhites ((col,sta):rest) = if col == White then ((col,sta): getWhites rest) else getWhites rest 
   
   getBlacks :: [Piece] -> [Piece]
   getBlacks [] = []
   getBlacks ((col,sta):rest) = if col == Black then ((col,sta): getBlacks rest) else getBlacks rest 
   
   showCapturedList :: [Piece] -> String
   showCapturedList [] = []
   showCapturedList (h:t) = (showPiece h :(showCapturedList t)) 
                                          
-------------------- IO Section --------------------
   main = do
      let (board, eatList) = ((Map.fromList (readBoard chessBoard)), eatenList)
      handle <- openFile "a3-moves.txt" ReadMode
      processMoves handle (board, eatList)
      hClose handle
      putStrLn "End of processing"

   processMoves :: Handle -> (Board,[Piece]) -> IO ()
   processMoves handle (board,eList) = do
      inEOF <- hIsEOF handle
      if ( inEOF ) 
      then return ()
      else do
         oneMove <- hGetLine handle
         when ( not $ null oneMove ) $ do
            putStrLn ("Current move is --- " ++ oneMove)
            let pos = parseInputPosition oneMove
                dir = parseInputDirection oneMove
                size = parseInputSize oneMove
                moveTuple = makeMove board pos dir size eList
                whiteList = getWhites $ snd moveTuple 
                blackList = getBlacks $ snd moveTuple 
            showBoard $ fst moveTuple 
--            putStrLn $ "move dir is " ++ dir
--            putStrLn $ "move size is " ++ size
            putStrLn $ "White pieces captured: " ++ showCapturedList whiteList
            putStrLn $ "Black pieces captured: " ++ showCapturedList blackList
            putStr "Hit \"Enter\" key to advance to the next move "
            anyKey <- getLine
            processMoves handle moveTuple

         
