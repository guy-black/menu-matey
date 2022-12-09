{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Text as T
import UCFML
import Monomer
import Control.Exception
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
    RawFile t -> label_ t [multiline]
    ParseError t -> label t
    ParsedFile u -> label "file pased successfully!"

checkFile :: (Either SomeException String) -> UCFMLFile
checkFile (Left _) = NotAFile
checkFile (Right s) = RawFile $ T.pack s

main :: IO ()
main = do
  args <- getArgs
-- set model to RawFile until parser is finished writing then
-- if parse failed, set model to ParseError
-- if parse succeeded, set model to ParsedFile
  file <- try (readFile $ head args)
  startApp (AppModel (checkFile file)) handleEvent buildUI config where
    config =
      [ appWindowTitle "menumatey"
      , appInitEvent AppInit
      , appTheme darkTheme
      , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
      ]
