module Main (main) where

import Test.Hspec
import ProtocolSpec (spec)


main :: IO ()
main = do
  hspec $ do describe "Protocol module" spec

