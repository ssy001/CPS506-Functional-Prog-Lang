%% To run the program, 
%% 1-load in erl REPL. 
%% 2-call function start()
%% NOTE: function main(...) performs some input error check for valid commands such as "Q", "d2 S", "d2 S 7", etc.

-module(a5).
-export([start/0]).
%-compile([export_all]).
%-import(piece.erl, [loop/0]).

%%% Data Structures
initChessBoard() ->
    [{a8,"r"}, {b8,"n"}, {c8,"b"}, {d8,"q"}, 
     {e8,"k"}, {f8,"b"}, {g8,"n"}, {h8,"r"}, 
     {a7,"p"}, {b7,"p"}, {c7,"p"}, {d7,"p"}, 
     {e7,"p"}, {f7,"p"}, {g7,"p"}, {h7,"p"}, 
     {a6," "}, {b6," "}, {c6," "}, {d6," "}, 
     {e6," "}, {f6," "}, {g6," "}, {h6," "}, 
     {a5," "}, {b5," "}, {c5," "}, {d5," "}, 
     {e5," "}, {f5," "}, {g5," "}, {h5," "}, 
     {a4," "}, {b4," "}, {c4," "}, {d4," "}, 
     {e4," "}, {f4," "}, {g4," "}, {h4," "}, 
     {a3," "}, {b3," "}, {c3," "}, {d3," "}, 
     {e3," "}, {f3," "}, {g3," "}, {h3," "},
     {a2,"P"}, {b2,"P"}, {c2,"P"}, {d2,"P"}, 
     {e2,"P"}, {f2,"P"}, {g2,"P"}, {h2,"P"}, 
     {a1,"R"}, {b1,"N"}, {c1,"B"}, {d1,"Q"}, 
     {e1,"K"}, {f1,"B"}, {g1,"N"}, {h1,"R"}].

blackPieces() -> ["k","q","b","n","r","p"].
%whitePieces() -> ["K","Q","B","N","R","P"].

validMoves("k") -> ["N","NE","E","SE","S","SW","W","NW"];
validMoves("q") -> ["N","NE","E","SE","S","SW","W","NW"];
validMoves("b") -> ["NE","SE","SW","NW"];
validMoves("n") -> ["NNW","NNE","NWW","NEE","SSW","SSE","SWW","SEE"];
validMoves("r") -> ["N","E","S","W"];
validMoves("p") -> ["SE","S","SW"];
validMoves("K") -> ["N","NE","E","SE","S","SW","W","NW"];
validMoves("Q") -> ["N","NE","E","SE","S","SW","W","NW"];
validMoves("B") -> ["NE","SE","SW","NW"];
validMoves("N") -> ["NNW","NNE","NWW","NEE","SSW","SSE","SWW","SEE"];
validMoves("R") -> ["N","E","S","W"];
validMoves("P") -> ["NE","N","NW"];
validMoves(" ") -> [].

sizeMoves("k") -> 1;
sizeMoves("q") -> 7;
sizeMoves("b") -> 7;
sizeMoves("n") -> 1;
sizeMoves("r") -> 7;
sizeMoves("p") -> 1;
sizeMoves("K") -> 1;
sizeMoves("Q") -> 7;
sizeMoves("B") -> 7;
sizeMoves("N") -> 1;
sizeMoves("R") -> 7;
sizeMoves("P") -> 1.

%%% (Actual) Functions
isValidMove(Piece, Dir) -> 
   Moves = validMoves(Piece),
   lists:member(Dir, Moves).
   
nextPos(Pos, Dir) ->
   File = hd(Pos), Rank = hd(tl(Pos)),
   case Dir of
      "N" -> binary_to_list(<<File>>) ++ binary_to_list(<<(Rank+1)>>);
      "S" -> binary_to_list(<<File>>) ++ binary_to_list(<<(Rank-1)>>);
      "E" -> binary_to_list(<<(File+1)>>) ++ binary_to_list(<<Rank>>);
      "W" -> binary_to_list(<<(File-1)>>) ++ binary_to_list(<<Rank>>);
      "NE" -> binary_to_list(<<(File+1)>>) ++ binary_to_list(<<(Rank+1)>>);
      "NW" -> binary_to_list(<<(File-1)>>) ++ binary_to_list(<<(Rank+1)>>);
      "SE" -> binary_to_list(<<(File+1)>>) ++ binary_to_list(<<(Rank-1)>>);
      "SW" -> binary_to_list(<<(File-1)>>) ++ binary_to_list(<<(Rank-1)>>);
      "NNE" -> binary_to_list(<<(File+1)>>) ++ binary_to_list(<<(Rank+2)>>);
      "NNW" -> binary_to_list(<<(File-1)>>) ++ binary_to_list(<<(Rank+2)>>);
      "NEE" -> binary_to_list(<<(File+2)>>) ++ binary_to_list(<<(Rank+1)>>);
      "NWW" -> binary_to_list(<<(File-2)>>) ++ binary_to_list(<<(Rank+1)>>);
      "SSE" -> binary_to_list(<<(File+1)>>) ++ binary_to_list(<<(Rank-2)>>);
      "SSW" -> binary_to_list(<<(File-1)>>) ++ binary_to_list(<<(Rank-2)>>);
      "SEE" -> binary_to_list(<<(File+2)>>) ++ binary_to_list(<<(Rank-1)>>);
      "SWW" -> binary_to_list(<<(File-2)>>) ++ binary_to_list(<<(Rank-1)>>);
      _     -> Pos
   end.

isInBounds(Pos) ->
   File = hd(Pos), Rank = hd(tl(Pos)),
   if File >= 97, File =< 104, Rank >= 49, Rank =< 56 -> true;
      true -> false
   end. 

isSameColor(Pc1, Pc2) -> 
   BlPc = blackPieces(),
   %WhPc = whitePieces(),
   IsPc1Black = lists:any(fun(X) -> X == Pc1 end, BlPc),
   IsPc2Black = lists:any(fun(X) -> X == Pc2 end, BlPc),
   if IsPc1Black == true, IsPc2Black == true -> true;
      IsPc1Black =/= true, IsPc2Black =/= true -> true;
      IsPc1Black == true, IsPc2Black =/= true -> false;
      IsPc1Black =/= true, IsPc2Black == true -> false;
      true -> false
   end.

getPosPiece(Board, PosStr) -> 
   Pos = list_to_atom(PosStr),
   [{Pos,Piece}|_] = [{Po,Pc} || {Po,Pc} <- Board, Po =:= Pos],
   (Piece).

showBoard(Board) ->
   Files = ["a","b","c","d","e","f","g","h"],
   Ranks = ["8","7","6","5","4","3","2","1"],
   FileRank = [F++R || R <- Ranks, F <- Files],
   io:format("  -----------------------------------~n"),
   Fn = fun(Pos) ->
      Piece = getPosPiece(Board,Pos),
      FirstChar = hd(Pos),
      if FirstChar >= 104 -> io:format("|~p|~n", [Piece]);
         FirstChar =< 97  -> io:format("~p |~p", [tl(Pos),Piece]);
         true            -> io:format("|~p", [Piece])
      end
   end,
   lists:foreach(Fn, FileRank),
   io:format("  -----------------------------------~n"),
   io:format("      a   b   c   d   e   f   g   h~n").

findBestPos(Board,Pos,Dir,Piece,MoveSize) ->
   NewPos = nextPos(Pos,Dir),
   InBounds = isInBounds(NewPos),
   if InBounds == true, MoveSize >= 1 -> 
         NewPosPiece = getPosPiece(Board,NewPos),
         if NewPosPiece == " " -> findBestPos(Board,NewPos,Dir,Piece,(MoveSize-1));
            true               -> SameColor = isSameColor(Piece,NewPosPiece),
                                  if SameColor == true -> Pos;
                                     true              -> NewPos    %if 
                                  end
         end;
      true -> Pos    %no more moves available or NewPos not inbounds
   end.      

addPieceToTaken(TargetPiece,BlTaken,WhTaken) -> 
   BlPc = blackPieces(),
   IsPieceBlack = lists:any(fun(X) -> X == TargetPiece end, BlPc),
   if IsPieceBlack == true -> {BlTaken++[TargetPiece], WhTaken};
      true -> {BlTaken, WhTaken++[TargetPiece]}
   end.

moveToPos(Board,Pos,BestPos,BestPosPiece) ->
   PosA = list_to_atom(Pos),
   BestPosA = list_to_atom(BestPos),
   Board1 = [{PosAtom, PcStr} || {PosAtom, PcStr} <- Board, PosAtom =/= PosA],
   Board2 = [{PosAtom, PcStr} || {PosAtom, PcStr} <- Board1, PosAtom =/= BestPosA],
   Board3 = Board2 ++ [{PosA," "}],
   Board4 = Board3 ++ [{BestPosA,BestPosPiece}],
   (Board4).

movePiece(Board,Pos,Dir,Size,BlTaken,WhTaken) ->
   Piece = getPosPiece(Board,Pos),
   IsValidM = isValidMove(Piece,Dir),
   if IsValidM == true, Piece =/= " " -> 
         MaxMoves = lists:min([Size,sizeMoves(Piece)]),
         BestPos = findBestPos(Board,Pos,Dir,Piece,MaxMoves),
         if BestPos =/= Pos ->
               BestPosPiece = getPosPiece(Board,BestPos),
               SameColor = isSameColor(Piece,BestPosPiece),           %also works if no BestPosPiece (ie. empty)
               {NewBlTaken,NewWhTaken} = if BestPosPiece =/= " ", SameColor == false -> 
                                               addPieceToTaken(BestPosPiece,BlTaken,WhTaken);
                                            true -> {BlTaken, WhTaken}
                                         end,
               NewBoard = moveToPos(Board,Pos,BestPos,Piece);  %move Piece to BestPos, put " " on Pos
            true -> NewBoard = Board, NewBlTaken = BlTaken, NewWhTaken = WhTaken
         end,
         [NewBoard,NewBlTaken,NewWhTaken];
      true -> [Board,BlTaken,WhTaken]                  %if move is not valid OR no piece (empty) at Pos   
   end.

%% processes inputs and makes appropriate moves
main(Board,BlTaken,WhTaken) ->
   showBoard(Board),
   io:format("Black Taken: ~p~n", [BlTaken]),
   io:format("White Taken: ~p~n", [WhTaken]),
   Command = io:get_line("Input your move: "),
   Args = string:tokens(Command, " \"|\" \n"),
   Pos = hd(Args), 
   {Direc,MoveSize} = 
   if Pos == "Q"; length(Args) == 1 -> {"NA", 0};
      length(Args) == 2             -> {hd(tl(Args)), 7};
      length(Args) == 3             -> {Size,_} = string:to_integer(hd(tl(tl(Args)))),
                                       {hd(tl(Args)), Size};
      true                          -> {"NA", 0}
   end,
   if Pos =/= "Q" -> 
         [NewBoard,NewBlTaken,NewWhTaken] = movePiece (Board,Pos,Direc,MoveSize,BlTaken,WhTaken),
         main(NewBoard,NewBlTaken,NewWhTaken);
      true -> io:format("Goodbye.~n")
   end.

start() ->
   ChessBoard = initChessBoard(),
   main(ChessBoard, [], []).



