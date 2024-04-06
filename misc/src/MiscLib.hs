-- file: MiscLib.hs
-- author: Jacob Xie
-- date: 2024/03/02 21:53:55 Saturday
-- brief:

module MiscLib
  ( module MiscLib.CronSchema,
    readEnvFile,
    module MiscLib.CsvHelper,
    (!?),
  )
where

import MiscLib.CronSchema
import MiscLib.CsvHelper
import MiscLib.DotenvReader (readEnvFile)

-- safe `!!`
(!?) :: [a] -> Int -> Maybe a
{-# INLINEABLE (!?) #-}
xs !? n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n
