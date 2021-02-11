module TicTacToe where

import Prelude

import Control.Alternative ((<|>))
import Data.Either (Either(..), note)
import Data.List (List, catMaybes, concat, filter, findMap, head, index, length, nub, null, range, transpose, updateAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)

data Marker = X | O

derive instance eqMarker :: Eq Marker
derive instance ordMarker :: Ord Marker

instance showMarker :: Show Marker where
  show X = "X"
  show O = "O"

type Tile = Maybe Marker
type Position = Tuple Int Int

type Board = List (List Tile)

data GameOver 
  = Tied
  | Victory Marker

instance showGameOver :: Show GameOver where
  show Tied  = "It's a tie!"
  show (Victory m) = "The winner is: " <> show m

type GameState = {
  currentTurn :: Marker,
  board :: Board,
  outcome :: Maybe GameOver
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
playAt pos@(Tuple rowPos colPos) game@{currentTurn, board, outcome} =
  stillInProgress 
    <* tileIsFree 
    *> makeMove
    >>= updateState
  where
    stillInProgress = maybe (pure unit) (const <<< Left $ "Game over!") outcome
    tileIsFree = maybe (pure unit) (const <<< Left $ "Invalid move") $ markerAt pos board
    makeMove = do
        row <- board !! rowPos # note "Invalid row"
        updatedRow <- updateAt colPos (Just currentTurn) row # note "Invalid column"
        updateAt rowPos updatedRow board # note "Invalid row"
    updateState updatedBoard = 
      pure $ {currentTurn: (next currentTurn), board: updatedBoard, outcome: determineOutcome updatedBoard}

determineOutcome :: Board -> Maybe GameOver
determineOutcome board = 
  if isTie board then
    Just Tied
  else
    Victory <$> findWinner board 

findWinner :: Board -> Maybe Marker
findWinner board =
  findMap rowVictor rows <|> findMap rowVictor cols <|> findMap rowVictor diagonals
  where
    rows = board
    cols = transpose board
    diagonals = 
      [map (\i -> markerAt (Tuple i  i) board) (range 0 2)
      , map (\i -> markerAt (Tuple (i-1) (3-i)) board) (range 1 3)]

rowVictor :: List Tile -> Maybe Marker
rowVictor r = 
  if length occupied == 3 && length (nub occupied) == 1 then
    head occupied
  else
    Nothing
  where
    occupied = catMaybes r

isTie :: Board -> Boolean
isTie = 
  concat >>> filter isNothing >>> null
