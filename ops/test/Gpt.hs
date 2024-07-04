-- file: Gpt.hs
-- author: Jacob Xie
-- date: 2024/05/27 11:12:22 Monday
-- brief:

module Main where

import OpsLib.RingBuffer

main :: IO ()
main = do
  let rb = newRingBuffer 3 :: RingBuffer Int

      rb1 = appendRingBuffer 1 rb
      rb2 = appendRingBuffer 2 rb1
      rb3 = appendRingBuffer 3 rb2
      rb4 = appendRingBuffer 4 rb3
      rb5 = appendRingBuffer 5 rb4
      rb6 = appendRingBuffer 6 rb5
      rb7 = appendRingBuffer 7 rb6

  putStrLn $ "rb: " <> show (getRingBuffer rb7)
