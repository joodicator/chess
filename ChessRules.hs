--------------------------------------------------------------------------------
module ChessRules where

import Control.Monad
import Control.Applicative
import Data.Maybe

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

promotions :: [Promotion]
promotions = [Rook, Knight, Bishop, Queen]

--------------------------------------------------------------------------------
base :: Colour -> Rank
base Black = head ranks
base White = last ranks

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
-- c is in check.
inCheck :: Board -> Colour -> Bool
inCheck d c
  = any (attacks d (oppose c)) (indices d (c,King))

-- j is under attack by a piece belonging to ic.
attacks :: Board -> Colour -> Index -> Bool
attacks d ic j
  = any isJust [tryBasicMove (ic,ip,(i,j)) d | (i,(_,ip)) <- list ic d]

--------------------------------------------------------------------------------
-- Succeeds with the move specified by (t,mpp) iff it is legal in this game.
tryMove :: (Path,Maybe Promotion) -> Game -> Maybe Move
tryMove (t@(i,j),mpp) g@Game{gBoard=d, gTurn=pc} = do
    guard (inBoard i && inBoard j)
    (ic,ip) <- d!i; guard (ic==pc)
    m <- tryBasicMove (ic,ip,t) d <|> trySpecialMove (ip,t) g
    m <- tryPromote (ic,ip,mpp) m
    guard (not $ inCheck (doMove m d) pc)
    return m

-- Pre: d!(ri,fi)==Just(ic,ip) -------------------------------------------------
-- A legal move, ignoring en passant, pawn promotion, turn order and check.
tryBasicMove :: (Colour,Piece,Path) -> Board -> Maybe Move
tryBasicMove s@(ic,ip,t@((ri,fi),(rj,fj))) d = case ip of
    Pawn ->
        (guard (rj==ri+1*fore ic && fj==fi) >>
         tryJustMove s d) <|>
        (guard (rj==ri+2*fore ic && fj==fi && ri==1+base ic) >>
         trySlide t d >> tryJustMove s d) <|>
        (guard (rj==ri+1*fore ic && abs(fj-fi)==1) >>
         tryJustCapture s d)
    Knight ->
        guard ((abs$rj-ri,abs$fj-fi)`elem`[(1,2),(2,1)]) >>
        tryMaybeCapture s d
    Rook ->
        guard (rj==ri || fj==fi) >>
        trySlide t d >> tryMaybeCapture s d
    Bishop ->
        guard (abs(unR$rj-ri)==abs(unF$fj-fi)) >>
        trySlide t d >> tryMaybeCapture s d
    Queen ->
        guard (rj==ri || fj==fi || abs(unR$rj-ri)==abs(unF$fj-fi)) >>
        trySlide t d >> tryMaybeCapture s d
    King ->
        guard (abs(rj-ri)<=1 && abs(fj-fi)<= 1) >>
        tryMaybeCapture s d

-- Pre: d!i==Just(ic,ip) -------------------------------------------------------
-- Given a legal move ignoring pawn promotion and check, produces
-- a legal move obeying pawn promotion and ignoring check.
tryPromote :: (Colour,Piece,Maybe Promotion) -> Move -> Maybe Move
tryPromote (ic,ip,mpp) m = case (ip,m) of
    (Pawn,Move{mPath=t@(_,(rj,_)), mCapture=mpc}) | rj==top ic -> do
        pp <- mpp; guard (pp `elem` promotions)
        return Promote{mPath=t, mCapture=mpc, mPromote=pp}
    _ -> do
        guard (isNothing mpp)
        return m

-- Pre: d!i==Just(pc,ip) where (ip,(i,j))=s; Game{gTurn=pc,gBoard=d}=g ----------
-- A legal castling or en passant move, ignoring pawn promotion and check
trySpecialMove :: (Piece,Path) -> Game -> Maybe Move
trySpecialMove s g = tryCastle s g <|> tryPassant s g

-- Pre: d!i==Just(pc,ip) --------------------------------------------------------
-- A legal castling move
tryCastle :: (Piece,Path) -> Game -> Maybe Move
tryCastle (ip,t) g@Game{gBoard=d, gTurn=pc, gMoves=ms} = do
    guard (ip==King && ri==rj && abs(fj-fi)==2)
    (ic',Rook) <- d!i'; guard (ic'==pc)
    guard (null (pastMoves i ms) && null (pastMoves i' ms))
    trySlide (i,i') d
    mapM_ (guard . not . attacks d (oppose pc)) [i,j',j]
    return Castle{mKing=t, mRook=t'}
  where
    (i@(ri,fi),j@(rj,fj)) = t
    t'@(i',j') = ((ri,(if fj<fi then head else last) files),(ri,(fi+fj)`div`2))

-- Pre: d!i==Just(pc,ip) --------------------------------------------------------
-- A legal /en passant/ capture, ignoring check and pawn promotion
tryPassant :: (Piece,Path) -> Game -> Maybe Move
tryPassant (ip,t) Game{gBoard=d, gTurn=pc, gMoves=ms} = do
    guard (ip==Pawn && rj==ri+fore pc && abs(fj-fi)==1)
    (jc',Pawn) <- d!j'; guard (jc'/=pc)
    Move{mPath=t''} <- listToMaybe ms; guard (t''==t')
    return Passant{mPath=t}
  where
    (i@(ri,fi),j@(rj,fj)) = t
    t'@(i',j') = ((rj+fore pc,fj),(rj-fore pc,fj))

-- Succeeds iff the squares strictly between i and j are all empty. ------------
trySlide :: Path -> Board -> Maybe ()
trySlide (i@(ri,fi), j@(rj,fj)) board
  = mapM_ (guard . isNothing . (board !)) path
  where
    (rd,fd) = (signum (rj-ri), signum (fj-fi))
    path = takeWhile (/=j) $ drop 1 $ iterate (\(r,f) -> (r+rd, f+fd)) i

-- Pre: d!i==Just(ic,_) where (ic,_,(i,j))=s -----------------------------------
-- Jumps from i to j, if j is empty or holds an opponent's piece.
tryMaybeCapture :: (Colour,Piece,Path) -> Board -> Maybe Move
tryMaybeCapture s d = tryJustMove s d <|> tryJustCapture s d

-- Jumps from i to j, if j is empty. -------------------------------------------
tryJustMove :: (Colour,Piece,Path) -> Board -> Maybe Move
tryJustMove (_,_,t@(i,j)) d = do
    guard (isNothing $ d!j)
    return Move{mPath=t, mCapture=Nothing}

-- Pre: d!i==Just(ic,_) where (i,j)=t ------------------------------------------
-- Jumps from i to j, if j holds an opponent's piece, capturing it
tryJustCapture :: (Colour,Piece,Path) -> Board -> Maybe Move
tryJustCapture (ic,_,t@(_,j)) d = do
    (jc,jp) <- d!j; guard (jc/=ic)
    return Move{mPath=t, mCapture=Just jp}
