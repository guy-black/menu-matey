{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Text as T
import Monomer
import System.Environment
    ( getArgs
    )


data AppModel = AppModel
    { ucfmlfile :: T.Text
    } deriving (Eq, Show)

data AppEvent = AppInit
              deriving (Eq, Show)

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model =
  if T.null (ucfmlfile model) then
    label "no argument passed"
  else
    label (ucfmlfile model)

main :: IO ()
main = do
  args <- getArgs
  let model = AppModel (T.pack (unlines args)) in -- TODO: only set to true if the first argument is a file that can be pased as a UCFMLModel
    startApp model handleEvent buildUI config where
      config =
        [ appWindowTitle "menumate"
        , appInitEvent AppInit
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
        ]
