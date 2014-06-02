--------------------------------------------------------------------------------
module ChessMinMaxAI where

import qualified Data.Set as S
import Data.Function
import Data.Maybe
import Data.List
import Data.Ord
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict

import qualified ChessBoard as B
import ChessData
import ChessRules
import Standard

--------------------------------------------------------------------------------
type MoveCount  = Down Int
type PieceValue = Int

data Value
  = Min
  | Lose  PieceValue MoveCount
  | Draw  PieceValue MoveCount
  | Cycle PieceValue MoveCount
  | Value PieceValue MoveCount
  | Win   MoveCount PieceValue
  | Max
  deriving (Eq, Ord, Show)

data EqGame = EqGame{
    eBoard   :: Board,
    eTurn    :: Colour,
    ePassant :: Maybe Index,
    eCastle  :: S.Set Index}
    deriving (Eq, Ord)

data SearchState = SearchState{
    sMoveLimit :: !Int,
    sNodeCount :: !Int }

initialState :: Int -> SearchState
initialState moveLimit = SearchState{
    sMoveLimit = moveLimit,
    sNodeCount = 0 }

--------------------------------------------------------------------------------
minMaxPlay :: Monad m => Int -> Game -> m Move
minMaxPlay l g = do
    Just m <- return $ minMaxPlay' l g
    return m

minMaxPlay' :: Int -> Game -> Maybe Move
minMaxPlay' l g@Game{gTurn=pc} = do
    (vms@(_:_),_) <- return $ minMaxPlay'' l g
    (_,(_,m):_) <- return $ maximumBy (compare `on` fst) vms
    return m

minMaxPlay'' :: Int -> Game -> ([(Value,[(Game,Move)])], SearchState)
minMaxPlay'' l g@Game{gTurn=pc}
  = runState (bestMove'' (Min,Max) 0 pc g es) (initialState l)
  where
    gs = catMaybes $ takeWhile isJust $ iterate (>>= undoMoveGame) (Just g)
    es = S.fromList $ map eqGame $ tail gs

--------------------------------------------------------------------------------
bestMove' :: (Value,Value)      -- (α,β) if c=pc else (β,α), for α-β pruning.
          -> Int                -- Number of moves simulated until this point.
          -> Colour             -- Player for whom value is represented.
          -> Game               -- Current game state.
          -> S.Set EqGame       -- Previous game states.
          -> State SearchState  -- Miscellaneous algorithm state.
             (Maybe (Value,     -- Value of chosen move, judged to be optimal.
                     [(Game,    -- The game state before each move in sequence.
                       Move)])) -- Move sequence starting with optimal move.
bestMove' p mc c g@Game{gTurn=pc} es = do
    vms <- bestMove'' p mc c g es
    case vms of
        _:_ -> return $ Just $ maximumBy (comparePC `on` fst) vms
        []  -> return Nothing
  where
    comparePC u v
      | c == pc   = compare u v
      | otherwise = compare v u

bestMove'' :: (Value,Value) -> Int -> Colour -> Game -> S.Set EqGame
           -> State SearchState [(Value,[(Game,Move)])]
bestMove'' p mc c g@Game{gTurn=pc} es = do
    s@SearchState{sMoveLimit=moveLimit} <- get
    put s{sNodeCount=1+sNodeCount s}
    case compare mc moveLimit of
        LT -> evaluate p (mc+1) (legalMoves g)
        _  -> return []
  where
    comparePC :: Value -> Value -> Ordering
    comparePC u v
      | c == pc   = compare u v
      | otherwise = compare v u

    evaluate :: (Value,Value) -> Int -> [Move]
             -> State SearchState [(Value,[(Game,Move)])]
    evaluate (best,worst) mc ms@(m:ms') = do
        (v,ms'') <- evaluate' (worst,best) mc m
        let best' = maximumBy comparePC [best,v]
            p'    = (best',worst)
        case LT of --comparePC v worst of
            LT -> ((v,ms'') :) <$> evaluate p' mc ms'
            _  -> return [(v,ms'')]
    evaluate _ _ []
      = return []

    evaluate' :: (Value,Value) -> Int -> Move
              -> State SearchState (Value,[(Game,Move)])
    evaluate' p mc m = case v' of
        _ | not toCycle -> do
            mvm <- bestMove' p mc c g' (S.insert e es)
            case mvm of
                Just (v,ms) -> return (v, (g,m):ms)
                Nothing     -> return (v', [(g,m)])
        Value v'' mc | not fromCycle -> do
            return $ (Cycle v'' mc, [(g,m)])
        _ | otherwise -> do
            return (v', [(g,m)])
      where
        toCycle   = S.member (eqGame g') es
        fromCycle = S.member e es
        e         = eqGame g
        g'        = doMoveGame m g
        v'        = gameValue c mc g'

--------------------------------------------------------------------------------
gameValue :: Colour -> Int -> Game -> Value
gameValue c mc g = case result g of
    Just Checkmate{rWinner=wc} | wc==c  -> Win   dmc v
    Just Stalemate                      -> Draw  v dmc
    Nothing                             -> Value v dmc
    Just Checkmate{}                    -> Lose  v dmc
  where
    dmc = Down mc
    v   = boardValue c g

boardValue :: Colour -> Game -> PieceValue
boardValue c game@Game{gBoard=board}
  = castleValue c game
  - max 0 (castleValue (oppose c) game - oMinus)
  + sum [pieceValue p | (_,(_,p)) <- B.list c board]
  - sum [max 0 (pieceValue p - oMinus) | (_,(_,p)) <- B.list (oppose c) board]
  where oMinus = 0

castleValue :: Colour -> Game -> PieceValue
castleValue c game@Game{gBoard=d, gMoves=ms}
  | length castles >= 2 = 6
  | length castles >= 1 = 3
  | otherwise           = 0
  where
    castles = do
        (k,(_,King)) <- B.list c d
        guard $ null $ pastMoves k ms
        (r,(_,Rook)) <- B.list c d
        guard $ null $ pastMoves r ms

pieceValue :: Piece -> PieceValue
pieceValue p = case p of
    King   -> 0
    Pawn   -> 5
    Knight -> 20
    Bishop -> 25
    Rook   -> 30
    Queen  -> 200

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
