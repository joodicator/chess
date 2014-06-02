{-# LANGUAGE
    FlexibleInstances, TypeSynonymInstances, GeneralizedNewtypeDeriving  #-}

module ChessData(
    module ChessData,
    module ChessBoard
) where

import Data.Monoid
import {-# SOURCE #-} ChessBoard(Board)

--------------------------------------------------------------------------------
newtype Rank = R{unR::Int} deriving (Eq, Ord, Num, Enum, Real, Integral)
newtype File = F{unF::Int} deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show Rank where show = show . unR
instance Show File where show = show . unF

type Index = (Rank,File)
type Path = (Index,Index)

ranks = map R [1..8] :: [Rank]
files = map F [1..8] :: [File]

(firstRank, lastRank) = (head ranks, last ranks)
(firstFile, lastFile) = (head files, last files)

rank = fst :: Index -> Rank
file = snd :: Index -> File

inBoard :: Index -> Bool
inBoard (r,f)
  = firstRank <= r && r <= lastRank &&
    firstFile <= f && f <= lastFile

squareColour :: Index -> Colour
squareColour (R r, F f)
  | odd (r+f) = White
  | otherwise = Black

--------------------------------------------------------------------------------
data Colour
  = White | Black
  deriving (Eq, Enum, Bounded, Show, Ord)

oppose :: Colour -> Colour
oppose White = Black
oppose Black = White

data Result
  = Checkmate{ rWinner::Colour } | Stalemate
  deriving (Eq, Show)

data Piece
  = Pawn | Rook | Knight | Bishop | Queen | King
  deriving (Eq, Enum, Bounded, Show, Ord)

type Promotion = Piece
type MoveSpec = (Path, Maybe Promotion)

data Move
  = Move    { mPath::Path, mCapture::Maybe Piece }
  | Promote { mPath::Path, mCapture::Maybe Piece, mPromote::Promotion }
  | Castle  { mKing::Path, mRook::Path }
  | Passant { mPath::Path }
  deriving Show

data Game
  = Game{ gBoard::Board, gTurn::Colour, gMoves::[Move] }
  deriving Show
