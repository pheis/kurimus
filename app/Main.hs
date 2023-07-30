{-# LANGUAGE OverloadedStrings #-}

module Main where

import MyLib (handleEcho, run)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  run handleEcho
