-- file: MiscLib.hs
-- author: Jacob Xie
-- date: 2024/03/02 21:53:55 Saturday
-- brief:

module MiscLib
  ( module MiscLib.CronSchema,
    readEnvFile,
    module MiscLib.CsvHelper,
    loopingList,
  )
where

import Data.List
import MiscLib.CronSchema
import MiscLib.CsvHelper
import MiscLib.DotenvReader (readEnvFile)

-- deprecated, newer version of GHC has already included
-- (!?) :: [a] -> Int -> Maybe a
-- {-# INLINEABLE (!?) #-}
-- xs !? n
--   | n < 0 = Nothing
--   | otherwise =
--       foldr
--         ( \x r k -> case k of
--             0 -> Just x
--             _ -> r (k - 1)
--         )
--         (const Nothing)
--         xs
--         n

loopingList :: (Eq a) => [a] -> a -> Int -> a
loopingList [] _ _ = error "Empty list"
loopingList xs y i = xs !! (loc `mod` length xs)
  where
    loc = case y `elemIndex` xs of
      Nothing -> error "Element not in the list!"
      Just i' -> i' + i