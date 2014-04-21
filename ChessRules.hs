--------------------------------------------------------------------------------
module ChessRules where

import Control.Monad
import Control.Applicative

import ChessData
import ChessBoard
import ChessText

--------------------------------------------------------------------------------
initialBoard :: Board
initialBoard = readBoardLines [
    "R N L Q K L N R 8",
    "P P P P P P P P 7",
    ". , . , . , . , 6",
    ", . , . , . , . 5",
    ". , . , . , . , 4",
    ", . , . , . , . 3",
    "p p p p p p p p 2",
    "r n l q k l n r 1",
    "a b c d e f g h  "]

initialGame :: Game
initialGame = Game{gBoard=initialBoard, gTurn=White, gMoves=[]}

--------------------------------------------------------------------------------
inCheck :: Colour -> Board -> Bool
inCheck = undefined

--------------------------------------------------------------------------------
doMove :: Move -> Board -> Board
doMove m d = d // doMove' m d

doMove' :: Move -> Board -> [Update]
doMove' m board = case m of
    Move{ mPath=t }                     -> path t
    Passant{ mPath=t@(i,j) }            -> ((rank i,file j),Nothing) : path t
    Promote{ mPath=(i,j), mPromote=p }  -> (i,Nothing) : (j,promote i p) : []
    Castle{ mKing=kt, mRook=rt }        -> path kt ++ path rt
  where
    path (i,j)  = (i,Nothing) : (j,board!i) : []
    promote i p = (\(c,_) -> (c,p)) <$> board!i

--------------------------------------------------------------------------------
undoMove :: Move -> Board -> Board
undoMove m d = d // undoMove' m d

undoMove' :: Move -> Board -> [Update]
undoMove' m board = case m of
    Move{ mPath=(i,j), mCapture=mp }    -> (j,free i mp) : (i,board!j) : []
    Promote{ mPath=(i,j), mCapture=mp } -> (j,free i mp) : (i,demote j) : []
    Passant{ mPath=t@(i@(r,_),(_,f)) }  -> ((r,f),free i (Just Pawn)) : unpath t
    Castle{ mKing=kt, mRook=rt }        -> unpath kt ++ unpath rt
  where
    unpath (i,j) = (j,Nothing) : (i,board!j) : []
    demote i     = (\(c,_) -> (c,Pawn)) <$> board!i
    free i mp    = (\(c,_) p -> (oppose c,p)) <$> board!i <*> mp

--------------------------------------------------------------------------------
tryMove :: (Path, Maybe Promotion) -> Game -> Maybe Move
tryMove (t@(i,j),mpp) game@Game{gBoard=board, gTurn=pc} = do
    (ic,ip) <- board!i
    guard (ic == pc)
    move <- tryPromote' (ip,mpp) game =<< tryMove' (ip,t) game
    guard (not $ inCheck pc $ doMove move board)
    return move

-- Pre: board!i==Just(pc,ip) where (i,j)=t
tryMove' :: (Piece, Path) -> Game -> Maybe Move
tryMove' (ip,t) g@Game{gBoard=board, gTurn=pc} = case ip of
    Pawn | (rd `div` up) `elem` [1,2] && fd==0 ->
        trySlide' t g >> tryJustMove' t g
    Pawn | rd==up && abs fd==1 ->
        tryJustCapture' t g <|> tryPassant' t g
    Rook | (rd==0) /= (fd==0) ->
        trySlide' t g >> tryMaybeCapture' t g
    Knight | abs rd+abs fd==3 && rd/=0 && fd/=0 ->
        tryMaybeCapture' t g
    Bishop | abs rd==abs fd && rd /= 0 ->
        trySlide' t g >> tryMaybeCapture' t g
    Queen | (rd==fd || rd==0 || fd==0) && (rd,fd)/=(0,0) ->
        trySlide' t g >> tryMaybeCapture' t g
    King | max (abs rd) (abs fd) == 1 ->
        tryJustMove' t g <|> tryCastle' t g
  where
    ((R ri,F fi),(R rj,F fj)) = t
    (rd, fd) = (rj-ri, fj-fi)
    up = case pc of White -> 1; Black -> -1    
    
trySlide' :: Path -> Game -> Maybe Move
trySlide' = undefined

tryJustMove' :: Path -> Game -> Maybe Move
tryJustMove' = undefined

tryJustCapture' :: Path -> Game -> Maybe Move
tryJustCapture' = undefined

tryMaybeCapture' :: Path -> Game -> Maybe Move
tryMaybeCapture' = undefined

tryPassant' :: Path -> Game -> Maybe Move
tryPassant' = undefined

tryCastle' :: Path -> Game -> Maybe Move
tryCastle' = undefined

-- Pre: (gBoard g)!i==(pc,ip) where (i,j)=mPath move
tryPromote' :: (Piece, Maybe Promotion) -> Game -> Move -> Maybe Move
tryPromote' (ip,mpp) g@Game{gTurn=pc} move = case (ip,mpp,move) of
    (Pawn, Nothing, Move{mPath=t}) -> do
        guard (rank (snd t) /= lastRank)
        return move
    (Pawn, Just pp, Move{mPath=t, mCapture=mpc}) -> do
        guard (rank (snd t) == lastRank)
        return Promote{mPath=t, mCapture=mpc, mPromote=pp}
    (_, Nothing, _) -> Just move
    (_, Just _, _)  -> Nothing
  where
    lastRank = case pc of White -> last ranks; Black -> head ranks
