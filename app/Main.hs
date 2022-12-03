{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Text as T
import UCFML
import Monomer
import System.Environment
    ( getArgs
    )


data AppModel = AppModel
    { ucfmlfile :: UCFMLFile
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
  case (ucfmlfile model) of
    NoFile -> label "hey you didn't pass a file in. maybe later I'll add a file select button or something."
    NotAFile -> label "hey, that's not a file"
    RawFile t -> label t
    ParseError t -> label t
    ParsedFile u -> label "file pased successfully!"

main :: IO ()
main = do
  args <- getArgs
-- todo
-- check if an argument was passed, if not set model to NoFile
-- check if passed argument is a real file, if not set model to NotAFile
-- set model to RawFile until parser is finished writing then
-- if parse failed, set model to ParseError
-- if parse succeeded, set model to ParsedFile

  startApp model handleEvent buildUI config where
    config =
      [ appWindowTitle "menumatey"
      , appInitEvent AppInit
      , appTheme darkTheme
      , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
      ]
