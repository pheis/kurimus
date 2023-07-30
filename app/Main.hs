{-# LANGUAGE OverloadedStrings #-}

module Main where

import MyLib (handleEcho, run)

main :: IO ()
main = do
  run handleEcho
