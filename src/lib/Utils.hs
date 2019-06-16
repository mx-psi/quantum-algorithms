module Utils(
  withError, chernoffSize,timesM,numbered, withRepeat, forEach, untilM, mostCommon, waitForKey, retry
  ) where

import Control.Monad
import Data.List
import Data.Function
import QuipperLib.Simulation
import System.IO

-- | The type of probabilities
type Probability = Double

-- | `chernoffSize delta p`
-- | outputs an `n` such that an algorithm with success probability `p`
-- | will output an incorrect answer with probability less than `delta`
chernoffSize :: Probability -> Probability -> Int
chernoffSize delta p
  | p > 0.5 && 0 < delta && delta < 1
  = ceiling $ log (1 / delta) / (2 * (p - 0.5) ^ 2)
  | otherwise
  = error "[chernoffSize] incorrect probabilities"

-- | Get the most common element on a list
mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort

-- | Run the majority algorithm on a random algorithm
-- a fixed number of times
withRepeat :: (Ord a) => Int -> IO a -> IO a
withRepeat times = fmap mostCommon . replicateM times

-- | Run the majority algorithm on a random algorithm
-- so as to provide an answer with given accuracy
withError :: (Ord a) => Probability -> Probability -> IO a -> IO a
withError p delta = withRepeat (chernoffSize delta p)

-- | Monadic for loop
-- | `mconcat` in the Kleisli monoid of a monad
timesM :: (Monad m) => Int -> (a -> m a) -> a -> m a
timesM n = foldr (>=>) pure . replicate n

-- |foldMap specialized to Kleisli maps
forEach :: (Monad m) => [b] -> (b -> a -> m a) -> (a -> m a)
forEach xs f = foldr ((>=>) . f) pure xs

-- | Number a list of items starting with 0
numbered :: [a] -> [(Int, a)]
numbered = zip [0 ..]

-- | Perform a monadic loop
untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p m = do
  x <- m
  if p x then pure x else untilM p m

-- | Retry until a predicate is fulfilled and report number of attempts
retry :: Monad m => (a -> Bool) -> m a -> m (Int, a)
retry p m = retry' p m 0
 where
  retry' p m n = n `seq` do
    x <- m
    if p x then pure (n + 1, x) else retry' p m (n + 1)

-- | Wait for a key press by the user
waitForKey :: IO ()
waitForKey = do
  waitable <- hIsOpen stdin
  when waitable $ putStrLn "Press any key to continue..." >> getChar >> putStrLn
    ""
