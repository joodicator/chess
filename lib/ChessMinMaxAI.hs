--------------------------------------------------------------------------------
module ChessMinMaxAI where

import qualified Data.Set as S
import Data.Function
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

import qualified ChessBoard as B
import ChessData
import ChessRules

--------------------------------------------------------------------------------
type PieceValue = Int

data Value
  = Min | Lose | Draw | Cycle PieceValue | Value PieceValue | Win | Max
  deriving (Eq, Ord)

instance Show Value where
    show Min       = "Min"
    show Lose      = "Lose"
    show Draw      = "Draw"
    show (Cycle v) = "Cycle " ++ show v
    show (Value v) = show v
    show Win       = "Win"
    show Max       = "Max"

data EqGame = EqGame{
    eBoard   :: Board,
    eTurn    :: Colour,
    ePassant :: Maybe Index,
    eCastle  :: S.Set Index}
    deriving (Eq, Ord)

--------------------------------------------------------------------------------
minMaxPlay :: Monad m => Int -> Game -> m Move
minMaxPlay l g = do
    Just m <- return $ minMaxPlay' l g
    return m

minMaxPlay' :: Int -> Game -> Maybe Move
minMaxPlay' l g@Game{gTurn=pc}
  = case minMaxPlay'' l g of
        ([],_)  -> Nothing
        (vms,_) -> Just . snd $ maximumBy (compare `on` fst) vms

minMaxPlay'' :: Int -> Game -> ([(Value,Move)], Int)
minMaxPlay'' l g@Game{gTurn=pc}
  = bestMove'' (Min,Max) l pc g es
  where
    gs = catMaybes $ takeWhile isJust $ iterate (>>= undoMoveGame) (Just g)
    es = S.fromList $ map eqGame $ tail gs

--------------------------------------------------------------------------------
type Eqs = S.Set EqGame

bestMove' :: (Value,Value)        -- (α,β) if c=pc else (β,α), for α-β pruning.
          -> Int                  -- Maximum remaining moves to examine.
          -> Colour               -- Player for whom value is represented.
          -> Game                 -- Current game state.
          -> Eqs                  -- Previous game states.
          -> (Maybe (Value,Move), -- Move judged optimal for current player.
              Int)                -- Unused move quota.
bestMove' p l c g@Game{gTurn=pc} es
  = case vms of
        _:_ -> (Just $ maximumBy (comparePC `on` fst) vms, l')
        []  -> (Nothing, l')
  where
    (vms, l') = bestMove'' p l c g es
    comparePC u v
      | c == pc   = compare u v
      | otherwise = compare v u

bestMove'' :: (Value,Value) -> Int -> Colour -> Game -> Eqs
           -> ([(Value,Move)], Int)
bestMove'' p l c g@Game{gTurn=pc} es
  = let ms = legalMoves g in evaluate p l ms ([], 0)
  where
    comparePC :: Value -> Value -> Ordering
    comparePC u v
      | c == pc   = compare u v
      | otherwise = compare v u

    evaluate :: (Value,Value) -> Int -> [Move]
             -> ([(Value,Move)], Int) -> ([(Value,Move)], Int)
    evaluate (best,worst) l ms@(m:ms') (vmsA,rA)
      | comparePC v worst == LT = evaluate p' r' ms' ((v,m):vmsA, rA)
      | otherwise               = ((v,m):vmsA, r'+rA)
      where
        p'    = (best',worst)
        l'    = min l (3 * l `div` length ms)
        r'    = l - l' + r
        (v,r) = evaluate' (worst,best) l' m
        best' = maximumBy comparePC [best,v]
    evaluate _ l [] (vmsA,rA) = (vmsA,l+rA)

    evaluate' :: (Value,Value) -> Int -> Move -> (Value, Int)
    evaluate' p l m = case v' of
          _         | not toCycle && l'>0 -> (maybe v' fst mm, r)
          _         | not toCycle         -> (v',              l')
          Value v'' | not fromCycle       -> (Cycle v'',       l')
          _         | otherwise           -> (v',              l')
      where
        toCycle   = S.member (eqGame g') es
        fromCycle = S.member e es
        l'        = l - 1
        (mm,r)    = bestMove' p l' c g' (S.insert e es)
        e         = eqGame g
        g'        = doMoveGame m g
        v'        = gameValue c g'

--------------------------------------------------------------------------------
gameValue :: Colour -> Game -> Value
gameValue c g@Game{gBoard=board} = case result g of
    Just Checkmate{rWinner=wc} | wc==c  -> Win
    Just Stalemate                      -> Draw
    Nothing                             -> Value $ boardValue c board
    Just Checkmate{}                    -> Lose

boardValue :: Colour -> Board -> PieceValue
boardValue c board
  = sum [pieceValue p | (_,(_,p)) <- B.list c board]
  - sum [pieceValue p | (_,(_,p)) <- B.list (oppose c) board]

pieceValue :: Piece -> PieceValue
pieceValue King   = 0
pieceValue Pawn   = 1
pieceValue Rook   = 5
pieceValue Knight = 5
pieceValue Bishop = 5
pieceValue Queen  = 20

--------------------------------------------------------------------------------
eqGame :: Game -> EqGame
eqGame Game{gBoard=board, gTurn=pc, gMoves=ms}
  = EqGame{eBoard=board, eTurn=pc, ePassant=mpi, eCastle=sci}
  where
    mpi = do
        Move{mPath=(i@(ri,fi),j@(rj,fj))} : _ <- return ms
        (_,Pawn) <- board B.! i
        guard (fj==fi && abs(rj-ri)==2)
        return j
    sci = S.fromList $ do
        (k,(c,King)) <- B.listAll board
        guard (null $ pastMoves k ms)
        (i,(_,Rook)) <- B.list c board
        guard (null $ pastMoves i ms)
        return i
