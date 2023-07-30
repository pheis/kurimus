module Env (Env (Env), fromInit, ourNodeId, getNextId) where

import Data.IORef (atomicModifyIORef, newIORef)
import Data.Text qualified as T
import Protocol (Body (Init), nodeId, nodeIds)

data Env = Env
  { getNextId :: IO Int,
    ourNodeId :: T.Text,
    otherNodeIds :: [T.Text]
  }

uniqueIdGenerator :: IO (IO Int)
uniqueIdGenerator = do
  r <- newIORef 0
  let getNextId = atomicModifyIORef r $ \prevId -> let nextId = prevId + 1 in (nextId, nextId)
  return getNextId

fromInit :: Body -> IO Env
fromInit Init {nodeId, nodeIds} = do
  nextId <- uniqueIdGenerator
  return
    Env
      { getNextId = nextId,
        ourNodeId = nodeId,
        otherNodeIds = nodeIds
      }
fromInit _ = error "Expected Init message as the first message"
