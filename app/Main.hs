{-# LANGUAGE OverloadedStrings #-}

module Main where

import MyLib qualified (run)
import Protocol

sampleMsg :: Msg
sampleMsg =
  Msg
    { src = "source_node",
      dest = "destination_node",
      msgId = Just 123,
      inReplyTo = Nothing,
      body = Echo {echo = "Hello, world!"}
    }

main :: IO ()
main = do
  print sampleMsg
  putStrLn "Hello, Haskell!"
  MyLib.run
