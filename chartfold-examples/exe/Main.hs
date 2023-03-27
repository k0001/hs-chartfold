module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.Canvas.CmdLine (multiMain)

import Chartfold.Examples.One qualified

--------------------------------------------------------------------------------

main :: IO ()
main = multiMain $ fmap (fmap (pad 1.1))
  [ ("1", Chartfold.Examples.One.d)
  ]

