{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Text
import Monomer

data AppModel = AppModel
    { fileLoaded :: Bool
    } deriving (Eq, Show)

data AppEvent = AppInit
              deriving (Eq, Show)

main :: IO ()
main = someFunc
