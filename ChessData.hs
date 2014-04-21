{-# LANGUAGE
    FlexibleInstances, TypeSynonymInstances, GeneralizedNewtypeDeriving  #-}

module ChessData(
    module ChessData,
    module ChessBoard
) where

import Data.Monoid
import {-# SOURCE #-} ChessBoard(Board)

--------------------------------------------------------------------------------
newtype Rank = R Int deriving (Eq, Ord, Num)
newtype File = F Int deriving (Eq, Ord, Num)

type Index = (Rank,File)
type Path = (Index,Index)

instance Monoid Index where
    mempty                  = (0,0)
    mappend (r,f) (r',f')   = (r+r', f+f')

ranks = map R [1..8] :: [Rank]
files = map F [1..8] :: [File]

rank = fst :: Index -> Rank
file = snd :: Index -> File

--------------------------------------------------------------------------------
data Colour
  = White | Black
  deriving (Eq, Enum, Bounded, Show)

oppose :: Colour -> Colour
oppose White = Black
oppose Black = White

data Piece
  = Pawn | Rook | Knight | Bishop | Queen | King
  deriving (Eq, Enum, Bounded, Show)

type Promotion = Piece

data Move
  = Move    { mPath::Path, mCapture::Maybe Piece }
  | Promote { mPath::Path, mCapture::Maybe Piece, mPromote::Promotion }
  | Passant { mPath::Path }
  | Castle  { mKing::Path, mRook::Path }

data Game
  = Game{ gBoard::Board, gTurn::Colour, gMoves::[Move] }

--------------------------------------------------------------------------------
squareColour :: Index -> Colour
squareColour (R r, F f)
  | odd (r+f) = Black
  | otherwise = White
