--------------------------------------------------------------------------------
module ChessRules where

import Control.Monad
import Control.Applicative
import Data.Maybe

import ChessData
import ChessBoard
import {-# SOURCE #-} ChessText

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

promotions :: [Promotion]
promotions = [Rook, Knight, Bishop, Queen]

--------------------------------------------------------------------------------
base :: Colour -> Rank
base White = head ranks
base Black = last ranks

top :: Colour -> Rank
top = base . oppose

fore :: Colour -> Rank
fore White = 1
fore Black = -1

pastMoves :: Index -> [Move] -> [Move]
pastMoves k ms = case ms of
    m@Move    {mPath=(i,j)} : ms' | j==k    -> m : pastMoves i ms'
    m@Promote {mPath=(i,j)} : ms' | j==k    -> m : pastMoves i ms'
    m@Passant {mPath=(i,j)} : ms' | j==k    -> m : pastMoves i ms'
    m@Castle  {mKing=(i,j)} : ms' | j==k    -> m : pastMoves i ms'
    m@Castle  {mRook=(i,j)} : ms' | j==k    -> m : pastMoves i ms'
    _ : ms'                                 -> pastMoves k ms'
    []                                      -> []

--------------------------------------------------------------------------------
doMoveGame :: Move -> Game -> Game
doMoveGame m g@Game{gBoard=d, gTurn=pc, gMoves=ms}
  = g{gBoard=doMoveBoard m d, gTurn=oppose pc, gMoves=m:ms}

undoMoveGame :: Game -> Maybe Game
undoMoveGame g@Game{gBoard=d, gTurn=pc, gMoves=ms} = do
    m:ms' <- ms
    return g{gBoard=undoMoveBoard m d, gTurn=oppose pc, gMoves=ms'}    

--------------------------------------------------------------------------------
doMoveBoard :: Move -> Board -> Board
doMoveBoard m d = d // doMoveBoard' m d

doMoveBoard' :: Move -> Board -> [Update]
doMoveBoard' m board = case m of
    Move{ mPath=t }                     -> path t
    Passant{ mPath=t@(i,j) }            -> ((rank i,file j),Nothing) : path t
    Promote{ mPath=(i,j), mPromote=p }  -> (i,Nothing) : (j,promote i p) : []
    Castle{ mKing=kt, mRook=rt }        -> path kt ++ path rt
  where
    path (i,j)  = (i,Nothing) : (j,board!i) : []
    promote i p = (\(c,_) -> (c,p)) <$> board!i

undoMoveBoard :: Move -> Board -> Board
undoMoveBoard m d = d // undoMoveBoard' m d

undoMoveBoard' :: Move -> Board -> [Update]
undoMoveBoard' m board = case m of
    Move{ mPath=(i,j), mCapture=mp }    -> (j,free i mp) : (i,board!j) : []
    Promote{ mPath=(i,j), mCapture=mp } -> (j,free i mp) : (i,demote j) : []
    Passant{ mPath=t@(i@(r,_),(_,f)) }  -> ((r,f),free i (Just Pawn)) : unpath t
    Castle{ mKing=kt, mRook=rt }        -> unpath kt ++ unpath rt
  where
    unpath (i,j) = (j,Nothing) : (i,board!j) : []
    demote i     = (\(c,_) -> (c,Pawn)) <$> board!i
    free i mp    = (\(c,_) p -> (oppose c,p)) <$> board!i <*> mp

--------------------------------------------------------------------------------
result :: Game -> Maybe Result
result g@Game{gTurn=c}
  | canMove g = Nothing
  | inCheck g = Just Checkmate{rWinner=oppose c}
  | otherwise = Just Stalemate

-- True iff it is possible for the current player to make a legal move.
canMove :: Game -> Bool
canMove = not . null . legalMoves

-- Exactly the moves that the current player may legally make.
legalMoves :: Game -> [Move]
legalMoves g@Game{gTurn=c, gBoard=d}
  = catMaybes [tryMove s g | s <- maybeMoves d c]

-- A superset of the moves that player c may legally make on board d.
maybeMoves :: Board -> Colour -> [(Path,Maybe Promotion)]
maybeMoves d c = concatMap (maybeMovesFrom' d) (list c d)

-- Pre: d!i==Just(c,p) ---------------------------------------------------------
maybeMovesFrom' :: Board -> (Index,(Colour,Piece)) -> [(Path,Maybe Promotion)]
maybeMovesFrom' d (i@(ri,fi),(c,p)) = case p of
    Pawn ->
        let (rb,rt,rf) = (base c,top c,fore c) in
        [(,) (i,(ri+2*rf,fi))  Nothing] <|>
        [(,) (i,(ri+rf,fi+fd)) Nothing | fd <- [-1,0,1]] <|>
        [(,) (i,(ri+rf,fi))    (Just pp) | pp <- promotions]
    Knight -> do
        (rd,fd) <- [(1,2),(2,1)]; rs <- [-1,1]; fs <- [-1,1]
        return $ (,) (i,(ri+rd*rs,fi+fd*fs)) Nothing
    King -> do
        rd <- [-1,0,1]; fd <- if rd==0 then [-3,-1,1,3] else [-1,0,1]
        return $ (,) (i,(ri+rd,fi+fd)) Nothing
    _ -> do
        rd <- [-1,0,1]; fd <- if rd==0 then [-1,1] else [-1,0,1]
        guard (p/=Rook || rd==0 || fd==0)
        guard (p/=Bishop || rd+fd==0 || rd-fd==0)
        let js = tail $ iterate (\(r,f) -> (r+R rd,f+F fd)) i
        let (js',js'') = span (\j -> isNothing (d!j) && inBoard j) js
        [(,) (i,j) Nothing | j <- js' ++ take 1 js'']

-- True iff the current player is in check. ------------------------------------
inCheck :: Game -> Bool
inCheck Game{gTurn=c, gBoard=d} = inCheck' d c

inCheck' :: Board -> Colour -> Bool
inCheck' d c = any (attacks d (oppose c)) (indices d (c,King))

-- True iff any piece belonging to c would be able to capture an opponent's ----
-- piece at j if one were present, ignoring turn order, en passant and check.
attacks :: Board -> Colour -> Index -> Bool
attacks d c j = or [couldCapture' (c,p,(i,j)) d | (i,(c,p)) <- list c d]

--------------------------------------------------------------------------------
-- Succeeds with the move specified by (t,mpp) iff it is legal in this game.
tryMove :: (Path,Maybe Promotion) -> Game -> Maybe Move
tryMove (t@(i,j),mpp) g@Game{gBoard=d, gTurn=pc} = do
    guard (inBoard i && inBoard j)
    (ic,ip) <- d!i; guard (ic==pc)
    m <- tryBasicMove' (ic,ip,t) d <|> trySpecialMove' (ip,t) g
    m <- tryPromote' (ic,ip,mpp) m
    guard (not $ inCheck' (doMoveBoard m d) pc)
    return m

-- A legal move, ignoring en passant, pawn promotion, turn order and check -----
-- Pre: d!(ri,fi)==Just(ic,ip)
tryBasicMove' :: (Colour,Piece,Path) -> Board -> Maybe Move
tryBasicMove' s@(ic,ip,t@(i,j)) d = case d!j of
    Nothing -> do
        guard (couldJustMove' s d)
        return Move{mPath=t, mCapture=Nothing}
    Just (jc,jp) -> do
        guard (jc/=ic && couldCapture' s d)
        return Move{mPath=t, mCapture=Just jp}

-- Pre: d!i==Just(ic,ip) -------------------------------------------------------
-- Given a legal move ignoring pawn promotion and check, produces
-- a legal move obeying pawn promotion and ignoring check.
tryPromote' :: (Colour,Piece,Maybe Promotion) -> Move -> Maybe Move
tryPromote' (ic,ip,mpp) m = case (ip,m) of
    (Pawn,Move{mPath=t@(_,(rj,_)), mCapture=mpc}) | rj==top ic -> do
        pp <- mpp; guard (pp `elem` promotions)
        return Promote{mPath=t, mCapture=mpc, mPromote=pp}
    _ -> do
        guard (isNothing mpp)
        return m

-- Pre: d!i==Just(pc,ip) where (ip,(i,j))=s; Game{gTurn=pc,gBoard=d}=g ----------
-- A legal castling or en passant move, ignoring pawn promotion and check
trySpecialMove' :: (Piece,Path) -> Game -> Maybe Move
trySpecialMove' s g = tryCastle' s g <|> tryPassant' s g

-- Pre: d!i==Just(pc,ip) --------------------------------------------------------
-- A legal castling move
tryCastle' :: (Piece,Path) -> Game -> Maybe Move
tryCastle' (ip,t) g@Game{gBoard=d, gTurn=pc, gMoves=ms} = do
    guard (ip==King && ri==rj && abs(fj-fi)==2)
    (ic',Rook) <- d!i'; guard (ic'==pc)
    guard (null (pastMoves i ms) && null (pastMoves i' ms))
    guard (couldSlide (i,i') d)    
    mapM_ (guard . not . attacks d (oppose pc)) [i,j',j]
    return Castle{mKing=t, mRook=t'}
  where
    (i@(ri,fi),j@(rj,fj)) = t
    t'@(i',j') = ((ri,(if fj<fi then head else last) files),(ri,(fi+fj)`div`2))

-- Pre: d!i==Just(pc,ip) -------------------------------------------------------
-- A legal en passant capture, ignoring check and pawn promotion
tryPassant' :: (Piece,Path) -> Game -> Maybe Move
tryPassant' (ip,t) Game{gBoard=d, gTurn=pc, gMoves=ms} = do
    guard (ip==Pawn && rj==ri+fore pc && abs(fj-fi)==1)
    (jc',Pawn) <- d!j'; guard (jc'/=pc)
    Move{mPath=t''} <- listToMaybe ms; guard (t''==t')
    return Passant{mPath=t}
  where
    (i@(ri,fi),j@(rj,fj)) = t
    t'@(i',j') = ((rj+fore pc,fj),(rj-fore pc,fj))

-- Pre: d!i==Just(ic,ip) where (ic,ip,(i,j))=s ---------------------------------
-- True iff moving from i to j would be legal if j were empty OR if it contained
-- an opponent's piece, or both, ignoring en passant, turn order and check.
couldMove' :: (Colour,Piece,Path) -> Board -> Bool
couldMove' s d
  = couldMoveAndCapture' s d || couldOnlyMove' s d || couldOnlyCapture' s d

-- Pre: d!i==Just(ic,ip) where (ic,ip,(i,j))=s ---------------------------------
couldJustMove' :: (Colour,Piece,Path) -> Board -> Bool
couldJustMove' s d = couldMoveAndCapture' s d || couldOnlyMove' s d

-- Pre: d!i==Just(ic,ip) where (ic,ip,(i,j))=s ---------------------------------
couldCapture' :: (Colour,Piece,Path) -> Board -> Bool
couldCapture' s d = couldMoveAndCapture' s d || couldOnlyCapture' s d

-- Pre: d!i==Just(ic,ip) -------------------------------------------------------
-- True iff moving from i to j would be legal ONLY if j were empty,
-- ignoring turn order, en passant and check.
couldOnlyMove' :: (Colour,Piece,Path) -> Board -> Bool
couldOnlyMove' (c,p,t@((ri,fi),(rj,fj))) d = case p of
    Pawn -> fj==fi && (rj==ri+rf || (ri,rj)==(rb+1,ri+2*rf)) && couldSlide t d
    _    -> False
  where (rb,rt,rf) = (base c, top c, fore c)

-- Pre: d!i==Just(ic,ip) -------------------------------------------------------
-- True iff moving from i to j would be legal ONLY if an opponent's piece were
-- at j, ignoring turn order and check.
couldOnlyCapture' :: (Colour,Piece,Path) -> Board -> Bool
couldOnlyCapture' s@(ic,ip,t@((ri,fi),(rj,fj))) d = case ip of
    Pawn -> rj==ri+1*fore ic && abs(fj-fi)==1
    _    -> False

-- Pre: d!i==Just(c,p) ---------------------------------------------------------
-- True iff moving from i to j would be legal if j were empty AND if it
-- contained an opponent's piece, ignoring turn order and check.
couldMoveAndCapture' :: (Colour,Piece,Path) -> Board -> Bool
couldMoveAndCapture' (c,p,t@((ri,fi),(rj,fj))) d = case p of
    Pawn   -> False
    Knight -> (abs rd,abs fd) `elem` [(1,2),(2,1)]
    Rook   -> (rd==0 || fd==0) && couldSlide t d
    Bishop -> (rd-fd==0 || rd+fd==0) && couldSlide t d
    Queen  -> (rd==0 || fd==0 || rd+fd==0 || rd-fd==0) && couldSlide t d
    King   -> abs rd < 2 && abs fd < 2
  where (rd,fd) = (unR(rj-ri), unF(fj-fi))

-- True iff every square strictly between i and j is empty ---------------------
couldSlide :: Path -> Board -> Bool
couldSlide (i@(ri,fi),j@(rj,fj)) d
  = all (isNothing . (d !)) path
  where
    (rd,fd) = (signum (rj-ri), signum (fj-fi))
    path = takeWhile (/=j) $ tail $ iterate (\(r,f) -> (r+rd, f+fd)) i
