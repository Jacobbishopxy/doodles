-- file: RingBuffer.hs
-- author: Jacob Xie
-- date: 2024/05/27 13:50:47 Monday
-- brief:

module OpsLib.RingBuffer
  ( RingBuffer,
    newRingBuffer,
    newRingBuffer',
    appendRingBuffer,
    clearRingBuffer,
    getRingBuffer,
  )
where

import Control.Monad
import qualified Data.Vector as V

data RingBuffer a = RingBuffer
  { maxSize :: Int,
    cacheSize :: Int,
    currentSize :: Int,
    buffer :: V.Vector a
  }

instance (Show a) => Show (RingBuffer a) where
  show (RingBuffer _ _ _ b) = show b

-- | Create a new empty RingBuffer
newRingBuffer :: Int -> RingBuffer a
newRingBuffer maxSz =
  RingBuffer
    { maxSize = maxSz,
      cacheSize = max 1 (maxSz `div` 2),
      currentSize = 0,
      buffer = V.empty
    }

newRingBuffer' :: Int -> Int -> RingBuffer a
newRingBuffer' maxSz cacheSz =
  RingBuffer
    { maxSize = maxSz,
      cacheSize = cacheSz,
      currentSize = 0,
      buffer = V.empty
    }

-- | Insert an element into the RingBuffer
appendRingBuffer :: RingBuffer a -> a -> RingBuffer a
appendRingBuffer rb x
  | currentSize rb < maxSize rb + cacheSize rb =
      rb
        { buffer = buffer rb `V.snoc` x,
          currentSize = currentSize rb + 1
        }
  | otherwise =
      rb
        { buffer = newBuffer,
          currentSize = maxSize rb
        }
  where
    newBuffer = dropCacheSize (cacheSize rb) $ buffer rb `V.snoc` x
    dropCacheSize n vec
      | V.length vec <= n = V.empty
      | otherwise = V.drop n vec

clearRingBuffer :: RingBuffer a -> RingBuffer a
clearRingBuffer = liftM2 newRingBuffer' maxSize cacheSize

getRingBuffer :: RingBuffer a -> V.Vector a
getRingBuffer rb = V.take (maxSize rb) . V.drop (currentSize rb - maxSize rb) $ buffer rb
