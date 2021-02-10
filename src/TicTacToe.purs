module TicTacToe where

import Prelude

import Control.Alternative ((<|>))
import Data.Array (catMaybes, findMap, fromFoldable, head, index, length, nub, range, replicate, updateAt, (!!))
import Data.Either (Either(..), note)
import Data.List (transpose)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

data Marker = X | O

derive instance eqMarker :: Eq Marker
derive instance ordMarker :: Ord Marker

instance showMarker :: Show Marker where
  show X = "X"
  show O = "O"

type Tile = Maybe Marker
type Position = Tuple Int Int

type Board = Array (Array Tile)

type GameState = {
  currentTurn :: Marker,
  board :: Board
}

blankBoard :: Board
blankBoard =
  rows
  where
    row = replicate 3 Nothing
    rows = replicate 3 row

next :: Marker -> Marker
next X = O
next O = X

markerAt :: Position -> Board -> Maybe Marker
markerAt (Tuple rowPos colPos) board = do
  row <- index board rowPos
  fromMaybe Nothing $ index row colPos

playAt :: Position -> GameState -> Either String GameState
playAt pos@(Tuple rowPos colPos) {currentTurn, board} =
  case (markerAt pos board) of
    Just _ -> Left "Invalid move"
    Nothing -> do
      row <- note "Invalid row" $ board !! rowPos
      updatedRow <- note "Invalid column" $ updateAt colPos (Just nextTurn) row
      updatedBoard <- note "Invalid row" $ updateAt rowPos updatedRow board
      pure $ {currentTurn: nextTurn, board: updatedBoard}
  where
    nextTurn = next currentTurn
