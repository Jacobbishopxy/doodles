-- file: ListRB.hs
-- author: Jacob Xie
-- date: 2024/06/20 11:15:53 Thursday
-- brief:

module OpsLib.ListRB
  ( ListRB,
    createList,
    getList,
    appendList,
    clearList,
  )
where

import Brick.Widgets.List
import qualified Data.Vector as Vec
import OpsLib.RingBuffer

newtype ListRB n e = ListRB
  { listRB :: (RingBuffer e, List n e)
  }

-- Name, height, maxSize of buffer
createList :: n -> Int -> Int -> ListRB n e
createList n h s = ListRB (newRingBuffer s, list n Vec.empty h)

getList :: ListRB n e -> List n e
getList = snd . listRB

appendList :: e -> ListRB n e -> ListRB n e
appendList e lrb =
  let (rb, l) = listRB lrb
      newRb = appendRingBuffer rb e
   in if wasElementDropped newRb
        then ListRB (newRb, listReplace (getRingBuffer newRb) Nothing l)
        else ListRB (newRb, listInsert (length l) e l)

clearList :: ListRB n e -> ListRB n e
clearList lrb =
  let (rb, l) = listRB lrb
   in ListRB (clearRingBuffer rb, listClear l)
