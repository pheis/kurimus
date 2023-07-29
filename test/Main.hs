module Main (main) where

import ProtocolSpec (spec)
import Test.Hspec

main :: IO ()
main = do
  hspec $ do describe "Protocol module" spec
