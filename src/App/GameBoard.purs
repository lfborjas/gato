module App.GameBoard where

import Data.Array (range)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid ((<>))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, bind, map, show, ($))
import TicTacToe (GameOver(..), GameState, Marker(..), blankBoard, markerAt, playAt)

type State = GameState 

data Action 
  = MarkAt (Tuple Int Int)
  | StartOver

component :: forall q i o m. (MonadEffect m) => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ ->  {currentTurn: X, board: blankBoard, outcome: Nothing}
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [
      HH.h1_ [HH.text cat],
      status,
      HH.table_ (renderRows),
      startOver state.outcome,
      HH.p_ [HH.a [HP.href "https://github.com/lfborjas/gato"] [HH.text "Source"]]
    ]
  where
    status =
      if isJust state.outcome then
        HH.text $ maybe "" show state.outcome
      else
        HH.text $ "Current turn: " <> show state.currentTurn
    cat =
      case state.outcome of
        Nothing -> "(^._.^)ﾉ"
        Just Tied -> "(=;ェ;=)"
        Just (Victory _) -> "~(=^‥^)ノ☆"
    startOver Nothing = HH.span_ []
    startOver _ = 
      HH.button [HE.onClick \_ -> Just StartOver] [HH.text "Start Over"]
    renderRows = renderRow `map` range 0 2
    renderRow ix = HH.tr_ $ renderCell ix `map` range 0 2
    renderCell row col =
      HH.td 
        [HE.onClick \_ ->  markIfEmpty (Tuple row col)]
        [HH.text $ maybe "" show $ markerAt (Tuple row col) state.board]
    markIfEmpty pos =
      case (markerAt pos state.board) of
        Nothing -> Just $ MarkAt pos
        Just _ -> Nothing

handleAction :: forall cs o m. (MonadEffect m) => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  StartOver ->
    H.modify_ _ {currentTurn = X, board = blankBoard, outcome = Nothing}
  MarkAt pos -> do
    st <- H.get
    case (playAt pos st) of
      Left err -> H.liftEffect $ log $ "Error: " <> err
      Right {currentTurn, board, outcome} -> 
        H.modify_ _ {currentTurn =  currentTurn, board = board , outcome = outcome}
