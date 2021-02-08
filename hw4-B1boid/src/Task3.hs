{-# LANGUAGE ConstraintKinds #-}

module Task3
  ( ConcurrentHashTable (..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  ) where

import Data.Hashable (Hashable (..))
import Control.Monad (forM_)
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTMVar, readTVar, writeTVar)
import qualified Data.Vector as V

-- | Constraints for key in hashtable
type ConstraintsForKey k = (Eq k, Hashable k)

-- | Initial size of new hashtable
initSize :: Int
initSize = 24

-- | Border factor before increasing the number of cells
kBorder :: Double
kBorder = 0.5

-- | Concurrent hashtable implementation
data ConcurrentHashTable k v = ConcurrentHashTable
  { size :: TVar Int
  , content :: TVar (V.Vector (TVar [(k,v)]))
  }

-- | Create new hashtable
newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  size <- newTVar 0
  cells <- V.replicateM initSize (newTVar [])
  content <- newTVar cells
  return $ ConcurrentHashTable size content

-- | Put value by the key into hashtable and resize table if needed
putCHT
  :: ConstraintsForKey k  -- ^ Constraints for key in hashtable
  => k  -- ^ Current key
  -> v  -- ^ Current value to add
  -> ConcurrentHashTable k v -- ^ Hashtable
  -> IO ()  -- ^ The return value
putCHT k v table = atomically $ do
  resizeContent table
  putValue table (k, v)

-- | Put value by the key into hashtable
putValue
  :: ConstraintsForKey k  -- ^ Constraints for key in hashtable
  => ConcurrentHashTable k v  -- ^ Hashtable
  -> (k, v)  -- ^ Current pair to add
  -> STM ()  -- ^ The return value
putValue table addingPair@(k, v) = do
  content <- readTVar $ content table
  let cellTVar = (content V.! (hash k `mod` V.length content))
  curCell <- readTVar $ cellTVar
  case changeValue curCell addingPair [] False of
    Just newCell -> writeTVar cellTVar newCell
    Nothing      -> do
      writeTVar cellTVar ((k, v) : curCell)
      curSize <- readTVar $ size table
      writeTVar (size table) (curSize + 1)

-- | Find and change the value by the key in a cell
changeValue
  :: ConstraintsForKey k  -- ^ Constraints for key in hashtable
  => [(k, v)]  -- ^ Current cell
  -> (k, v)  -- ^ Current pair to add
  -> [(k, v)]  -- ^ Accumulator cell
  -> Bool   -- ^ True if the key exist in the cell
  -> Maybe [(k, v)]  -- ^ Returns new cell with changed value
changeValue []                         _      acc isFound = if isFound then Just acc else Nothing
changeValue (firstEl : restEl) (k, v) acc isFound
  | fst firstEl == k = changeValue restEl (k, v) ((k, v) : acc)  True
  | otherwise        = changeValue restEl (k, v) (firstEl : acc) isFound

-- | Find and change the value by the key in a cell
updateCells
  :: ConstraintsForKey k  -- ^ Constraints for key in hashtable
  => ConcurrentHashTable k v  -- ^ Hashtable
  -> V.Vector (TVar [(k,v)])  -- ^ Old cells
  -> V.Vector (TVar [(k,v)])  -- ^ New cells
  -> STM ()  -- ^ The return value
updateCells table cellsOld cellsUpd = do
  cells <- V.forM cellsOld readTVar
  writeTVar (content table) cellsUpd
  forM_ (concat cells) (putValue table)

-- | Resize content of hashtable if needed
resizeContent
  :: ConstraintsForKey k   -- ^ Constraints for key in hashtable
  => ConcurrentHashTable k v   -- ^ Hashtable
  -> STM ()  -- ^ The return value
resizeContent table = do
  cellsOld <- readTVar $ content table
  size <- readTVar $ size table
  let curBorder = V.length cellsOld
  if (fromIntegral curBorder >= kBorder * (fromIntegral size))
    then return ()
    else do
      cellsUpd <- V.replicateM (2 * curBorder) (newTVar [])
      updateCells table cellsOld cellsUpd

-- | Get element by the given key
getCHT
  :: ConstraintsForKey k  -- ^ Constraints for key in hashtable
  => k  -- ^ Given key
  -> ConcurrentHashTable k v  -- ^ Hashtable
  -> IO (Maybe v)  -- ^ The return element
getCHT k table = atomically $ do
  content <- readTVar $ content table
  curCell <- readTVar $ (content V.! (hash k `mod` V.length content))
  return $ lookup k curCell

-- | Get amount of elements in hashtable
sizeCHT
  :: ConcurrentHashTable k v  -- ^ Hashtable
  -> IO Int  -- ^ The return amount of elements
sizeCHT table = atomically $ readTVar $ size table
