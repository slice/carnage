module Carnage.Internal.Squeeze
  ( Squeezed (..),
    squeezeBy,
  )
where

import Data.List.NonEmpty (groupBy)

data Squeezed a
  = Unique a
  | Squeezed {occurrences :: Int, result :: a}
  deriving (Eq, Show)

-- | Squeeze segments of consecutively list elements together according to a
-- comparator function.
squeezeBy ::
  -- | Returns whether adjacent elements are to be considered equal.
  (a -> a -> Bool) ->
  -- | The list of elements to squeeze.
  [a] ->
  [Squeezed a]
squeezeBy f = fmap (\case a :| [] -> Unique a; as@(a :| _) -> Squeezed (length as) a) . groupBy f
