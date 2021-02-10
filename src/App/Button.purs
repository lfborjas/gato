module App.Button where

import Prelude (Unit, bind, map, pure, show, unit, ($), (<$>))
import TicTacToe (GameState, Marker(..), blankBoard, markerAt, playAt)

import Data.Array (range)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = GameState

data Action = MarkAt (Tuple Int Int)

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { currentTurn: X, board: blankBoard }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [
      HH.table_
      [
        HH.tr_ (renderRow 0),
        HH.tr_ (renderRow 1),
        HH.tr_ (renderRow 2)
      ]
    ]
  where
    renderRow ix = renderCell ix `map` range 0 2
    renderCell row col =
      HH.td 
        [HE.onClick \_ ->  markIfEmpty (Tuple row col)]
        [HH.text $ fromMaybe "" (show <$> markerAt (Tuple row col) state.board)]
    markIfEmpty pos =
      case (markerAt pos state.board) of
        Nothing -> Just $ MarkAt pos
        Just _ -> Nothing

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  MarkAt pos -> do
    st <- H.get
    case (playAt pos st) of
      Left err -> pure unit
      Right {currentTurn, board} -> 
        H.modify_ _ {currentTurn= currentTurn, board= board}
