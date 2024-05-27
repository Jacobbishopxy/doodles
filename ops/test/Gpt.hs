-- file: Gpt.hs
-- author: Jacob Xie
-- date: 2024/05/27 11:12:22 Monday
-- brief:

module Main where

import OpsLib.RingBuffer

main :: IO ()
main = do
  let rb = newRingBuffer 3 :: RingBuffer Int

      rb1 = appendRingBuffer rb 1
      rb2 = appendRingBuffer rb1 2
      rb3 = appendRingBuffer rb2 3
      rb4 = appendRingBuffer rb3 4
      rb5 = appendRingBuffer rb4 5
      rb6 = appendRingBuffer rb5 6
      rb7 = appendRingBuffer rb6 7

  putStrLn $ "rb: " <> show (getRingBuffer rb7)
