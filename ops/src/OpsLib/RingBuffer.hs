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
    isEmpty,
    isFull,
    getCurrentSize,
    getCacheSize,
    getMaxSize,
    wasElementDropped,
    toList,
  )
where

import qualified Data.Vector as V

data RingBuffer a = RingBuffer
  { maxSize :: Int,
    cacheSize :: Int,
    currentSize :: Int,
    buffer :: V.Vector a,
    lastDropped :: Bool
  }

instance (Show a) => Show (RingBuffer a) where
  show = show . getRingBuffer

-- | Create a new empty RingBuffer with a specified maximum size
newRingBuffer :: Int -> RingBuffer a
newRingBuffer maxSz =
  RingBuffer
    { maxSize = maxSz,
      cacheSize = max 1 (maxSz `div` 2),
      currentSize = 0,
      buffer = V.empty,
      lastDropped = False
    }

-- | Create a new empty RingBuffer with specified maximum size and cache size
newRingBuffer' :: Int -> Int -> RingBuffer a
newRingBuffer' maxSz cacheSz =
  RingBuffer
    { maxSize = maxSz,
      cacheSize = cacheSz,
      currentSize = 0,
      buffer = V.empty,
      lastDropped = False
    }

-- | Insert an element into the RingBuffer
appendRingBuffer :: a -> RingBuffer a -> RingBuffer a
appendRingBuffer x rb
  | currentSize rb < maxSize rb + cacheSize rb =
      rb
        { buffer = buffer rb `V.snoc` x,
          currentSize = currentSize rb + 1,
          lastDropped = False
        }
  | otherwise =
      rb
        { buffer = V.drop (cacheSize rb) (buffer rb) `V.snoc` x,
          currentSize = maxSize rb + 1,
          lastDropped = True
        }

-- | Clear the RingBuffer
clearRingBuffer :: RingBuffer a -> RingBuffer a
clearRingBuffer rb = rb {currentSize = 0, buffer = V.empty}

-- | Get the contents of the RingBuffer as a Vector
getRingBuffer :: RingBuffer a -> V.Vector a
getRingBuffer rb = V.drop (currentSize rb - maxSize rb) $ buffer rb

-- | Check if the RingBuffer is empty
isEmpty :: RingBuffer a -> Bool
isEmpty rb = currentSize rb == 0

-- | Check if the RingBuffer is full
isFull :: RingBuffer a -> Bool
isFull rb = currentSize rb >= maxSize rb

-- | Get the current size of the RingBuffer
getCurrentSize :: RingBuffer a -> Int
getCurrentSize = currentSize

-- | Get the cache size of the RingBuffer
getCacheSize :: RingBuffer a -> Int
getCacheSize = cacheSize

-- | Get the maximum size of the RingBuffer
getMaxSize :: RingBuffer a -> Int
getMaxSize = maxSize

-- | Check if the last append operation dropped an element
wasElementDropped :: RingBuffer a -> Bool
wasElementDropped = lastDropped

-- Convert the RingBuffer to a list (useful for Foldable and Traversable implementations)
toList :: RingBuffer a -> [a]
toList rb = V.toList $ getRingBuffer rb

----------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance Functor RingBuffer where
  fmap f rb = rb {buffer = V.map f $ buffer rb}

instance Foldable RingBuffer where
  foldMap f rb = foldMap f $ toList rb
  foldr f z rb = foldr f z $ toList rb

instance Traversable RingBuffer where
  traverse f rb =
    RingBuffer
      (maxSize rb)
      (cacheSize rb)
      (currentSize rb)
      <$> traverse f (buffer rb)
      <*> pure (lastDropped rb)
