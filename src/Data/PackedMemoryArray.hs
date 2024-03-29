{-# OPTIONS_GHC -Wall -fdefer-typed-holes -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- TBD.
module Data.PackedMemoryArray where

import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Maybe (isJust, fromMaybe, catMaybes)
import           Prelude

-- import           Debug.Trace

-- * PMA type

-- | Packed-memory array.
data PMA k v = PMA
  { capacity              :: Int              -- ^ Total capacity of PMA.
  , segmentCapacity       :: Int              -- ^ Size of each segment.
  , height                :: Int              -- ^ Height of the binary tree for elements.
  , elements              :: Vector (Maybe (k, v)) -- ^ Vector of all cells (elements or gaps).
  , cardinality           :: Int              -- ^ Number of elements contained.
  , segmentsCnt           :: Int              -- ^ Number of segments
  , segmentsCardinalities :: Vector Int       -- ^ Number of elements contained in each segment.
  } deriving (Show, Eq, Foldable)


-- * Combining?

-- * Traversals
-- * Folds?

-- * filter
-- * Submap
-- * Indexed
-- * Min/Max




-- * Constants

minCapacity :: Int
minCapacity = 8

t_h :: Double
t_h = 0.75

-- t_0 :: Double
-- t_0 = 1.0

p_h :: Double
p_h = 0.5

-- p_0 :: Double
-- p_0 = 0.1


-- * Construction

-- init PMA with given capacity
initialize :: Int -> PMA k a
initialize c = PMA
  { capacity = capacity
  , segmentCapacity = capacity
  , height = 1
  , elements = Vector.replicate (capacity) Nothing
  , cardinality = 0
  , segmentsCnt = 1
  , segmentsCardinalities = Vector.singleton 0
  }
  where
    capacity = max 8 (hyperceil c)

-- | Empty packed-memory array.
--
-- prop> capacity empty == minCapacity
empty :: PMA k a
empty = initialize minCapacity


singleton :: k -> a -> PMA k a
singleton k a = PMA
  { capacity = capacity
  , segmentCapacity = capacity
  , height = 1
  , elements = Vector.singleton (Just (k, a)) <> Vector.replicate (capacity - 1) Nothing
  , cardinality = 1
  , segmentsCnt = 1
  , segmentsCardinalities = Vector.singleton 1
  }
  where
    capacity = hyperceil minCapacity

-- todo
-- fromSet :: (k -> a) -> Set k -> Map k a

fromList :: Ord k => [(k, a)] -> PMA k a
fromList pairs = foldr (uncurry insert) (initialize (length pairs)) pairs

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> PMA k a
fromListWith combine pairs = foldr (uncurry (insertWith combine)) (initialize (length pairs)) pairs

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k, a)] -> PMA k a
fromListWithKey combine pairs = foldr (uncurry (insertWithKey combine)) (initialize (length pairs)) pairs

-- todo
-- fromAscList :: Eq k => [(k, a)] -> Map k a
-- fromAscListWith :: Eq k => (a -> a -> a) -> [(k, a)] -> Map k a
-- fromAscListWithKey :: Eq k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
-- fromDistinctAscList :: [(k, a)] -> Map k a



-- * Insertion

insert :: Ord k => k -> a -> PMA k a -> PMA k a
insert key val pma = snd (insertLookupWithKey override key val pma)
  where
    override _key newVal _oldVal = newVal

insertWith :: Ord k => (a -> a -> a) -> k -> a -> PMA k a -> PMA k a
insertWith combine key val = insertWithKey droppingKey key val
  where
    droppingKey = const combine

insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> PMA k a -> PMA k a
insertWithKey combine key val pma = snd (insertLookupWithKey combine key val pma)

insertLookupWithKey :: forall k a. (Ord k) 
                    => (k -> a -> a -> a) 
                    -> k 
                    -> a 
                    -> PMA k a 
                    -> (Maybe a, PMA k a)
insertLookupWithKey combine key val pma =
    case lookupInsert of Nothing            -> (Nothing, updatedPMA)
                         Just (pma, oldVal) -> (Just oldVal, pma)
  where
    lookupInsert = do
      oldVal <- pma !? key
      let combined = combine key val oldVal 
      let insertIndex = (findPlaceIndex key pma)
      let toInsert = Just (key, combined)
      return (pma { 
        elements = Vector.update (elements pma) (Vector.singleton (insertIndex, toInsert)) 
        }, oldVal)

    updatedPMA = if (((segmentsCardinalities newPMA) Vector.! segmentId) == (segmentCapacity pma)) then (rebalance newPMA segmentId) else newPMA

    segmentId = findSegment pma key

    (elements', posToInsert) = findPos (elements pma) ((segmentCapacity pma)*(segmentId) + ((segmentsCardinalities pma) Vector.! segmentId))
      where
        findPos :: Vector (Maybe (k, a)) -> Int -> (Vector (Maybe (k, a)), Int)
        findPos vec pos = if (pos > 0) && ((Just key) < fmap fst (vec Vector.! (pos - 1)))
                        then findPos (Vector.update vec (Vector.fromList [(pos - 1, Nothing), (pos, vec Vector.! (pos - 1))])) (pos - 1)
                        else (vec, pos)

    newElements = Vector.update elements' (Vector.singleton (posToInsert, Just (key, val)))

    newPMA = PMA
            { capacity = capacity pma
            , segmentCapacity = segmentCapacity pma
            , height = height pma
            , elements = newElements
            , cardinality = (cardinality pma) + 1
            , segmentsCnt = segmentsCnt pma
            , segmentsCardinalities = Vector.update (segmentsCardinalities pma) (Vector.singleton (segmentId, ((segmentsCardinalities pma) Vector.! segmentId) + 1))
            }


-- * Delition / Update todo

-- delete :: Ord k => k -> Map k a -> Map k a
-- adjust :: Ord k => (a -> a) -> k -> PMA k a -> PMA k a
-- adjustWithKey :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
-- update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
-- updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
-- updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
-- alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a



-- * Query


lookup :: Ord k => k -> PMA k a -> Maybe a
lookup key pma = do
      Just (k, v) <- elementsVector Vector.!? index
      if k == key
        then Just v
        else Nothing
  where
    elementsVector = elements pma
    index = findPlaceIndex key pma


(!?) :: Ord k => PMA k a -> k -> Maybe a
(!?) = flip Data.PackedMemoryArray.lookup

-- todo
-- (!) :: Ord k => Map k a -> k -> a

findWithDefault :: Ord k => a -> k -> PMA k a -> a
findWithDefault def key pma = case (pma !? key) of Just val -> val
                                                   Nothing  -> def

member :: Ord k => k -> PMA k a -> Bool
member key pma = isJust (pma !? key)

notMember :: Ord k => k -> PMA k a -> Bool
notMember key pma = not (member key pma)

-- todo and others
-- lookupGE :: Ord k => k -> PMA k v -> Maybe (k, v)


-- | Return all elements in range [start, end)
lookupInRange :: Ord k => (k, k) -> PMA k a -> [(k, a)]
lookupInRange (start, end) pma = catMaybes (Vector.toList slice)
  where
    startInd = findPlaceIndex start pma
    endInd   = findPlaceIndex end pma
    slice = Vector.slice startInd (endInd - startInd) (elements pma)


null :: PMA k a -> Bool
null pma = (cardinality pma) == 0

size :: PMA k a -> Int
size pma = cardinality pma


-- * Convesrion

elems :: PMA k a -> [a]
elems = map snd . assocs

keys :: PMA k a -> [k]
keys = map fst . assocs 

assocs :: PMA k a -> [(k, a)]
assocs pma = catMaybes (Vector.toList (elements pma))

-- todo
-- keysSet :: Map k a -> Set k


-- * Lists

toList :: PMA k a -> [(k, a)]
toList = assocs




-- * Helpers

log2 :: Int -> Double
log2 value = logBase 2 (fromIntegral value)

-- Round up to closest power of 2
hyperceil :: Int -> Int
hyperceil value = 2 ^ (ceiling (log2 value))

-- Get the lower and upper segments for the given segment_id at the given level
-- level >= 1
-- (l, r) both inclusive
windowBounds :: PMA a k -> Int -> Int -> (Int, Int)
windowBounds pma pos level = (l, r)
  where
    windowCapacity = (segmentCapacity pma) * (2 ^ (level - 1))
    l = (pos `div` windowCapacity) * windowCapacity
    r = l + windowCapacity - 1

-- Get the lower (p_l) and upper (t_l) threshold for the windows at the given level
-- level >= 1
windowThresholds :: PMA a k -> Int -> (Double, Double)
windowThresholds pma level = (p_l, t_l)
  where
    h = height pma
    diff = fromIntegral (h - level) / fromIntegral h
    p_l = p_h - 0.25 * diff
    t_l = t_h + 0.25 * diff

resize :: PMA a k -> PMA a k
resize pma = PMA
  { capacity = newCapacity
  , segmentCapacity = newSegmentCapacity
  , height = floor (log2 (newCapacity `div` newSegmentCapacity)) + 1
  , elements = newElements
  , cardinality = cardinality pma
  , segmentsCnt = newSegmentsCnt
  , segmentsCardinalities = newSegmentsCardinalities
  }
    where
      newCapacity = (capacity pma) * 2
      newSegmentCapacity = hyperceil (floor (log2 newCapacity))
      newSegmentsCnt = newCapacity `div` newSegmentCapacity
      elementsPerSegment = (cardinality pma) `div` newSegmentsCnt
      oddSegmentsCnt = (cardinality pma) `mod` newSegmentsCnt

      newSegmentsCardinalities = getSegmentsCardinalities 0
        where
          getSegmentsCardinalities :: Int -> Vector Int
          getSegmentsCardinalities i
            | i == newSegmentsCnt = Vector.empty
            | i < oddSegmentsCnt  = (Vector.singleton (elementsPerSegment + 1)) <> (getSegmentsCardinalities (i + 1))
            | otherwise           = (Vector.singleton elementsPerSegment) <> (getSegmentsCardinalities (i + 1))

      newElements = getElements (Vector.filter isJust (elements pma)) newSegmentsCardinalities
        where
          getElements :: Vector (Maybe a) -> Vector Int -> Vector (Maybe a)
          getElements elems sizes
            | Prelude.null sizes  = Vector.empty
            | otherwise   = curSegment <> (getElements (Vector.drop curSize elems) sizesTail)
            where
              curSize = sizes Vector.! 0
              sizesTail = Vector.drop 1 sizes
              curSegment = (Vector.take curSize elems) <> (Vector.replicate (newSegmentCapacity - curSize) Nothing)


-- Equally spread the elements in the interval [segmentCapacity * windowStart, segmentCapacity * (windowStart + windowLength) )
spread :: PMA k v -> Int -> Int -> Int -> PMA k v
spread pma elementsNum windowStart windowLength = pma
  { segmentsCardinalities = newSegmentsCardinalities
  , elements = newElements
  }
  where
    windowEnd = windowStart + windowLength

    tmp = getValidValues (subVector (elements pma) startPos len)
      where
        startPos = (segmentCapacity pma) * windowStart
        len = (segmentCapacity pma) * windowLength

        subVector :: Vector (Maybe a) -> Int -> Int -> Vector (Maybe a)
        subVector vec start n = Vector.take n (Vector.drop start vec)

        getValidValues :: Vector (Maybe a) -> Vector (Maybe a)
        getValidValues v = Vector.filter isJust v

    elementsPerSegment = elementsNum `div` windowLength
    oddSegmentsCnt = elementsNum `mod` windowLength

    newSubSegmentsCardinalities = getSegmentsCardinalities 0
      where
        getSegmentsCardinalities :: Int -> Vector Int
        getSegmentsCardinalities i
          | i == windowLength   = Vector.empty
          | i < oddSegmentsCnt  = (Vector.singleton (elementsPerSegment + 1)) <> (getSegmentsCardinalities (i + 1))
          | otherwise           = (Vector.singleton elementsPerSegment) <> (getSegmentsCardinalities (i + 1))

    newSubElements = getElements tmp newSubSegmentsCardinalities
      where
        getElements :: Vector (Maybe a) -> Vector Int -> Vector (Maybe a)
        getElements elems sizes
          | Prelude.null sizes  = Vector.empty
          | otherwise   = curSegment <> (getElements (Vector.drop curSize elems) sizesTail)
          where
            curSize = sizes Vector.! 0
            sizesTail = Vector.drop 1 sizes
            curSegment = (Vector.take curSize elems) <> (Vector.replicate ((segmentCapacity pma) - curSize) Nothing)

    newSegmentsCardinalities = replaceSubVector windowStart newSubSegmentsCardinalities (segmentsCardinalities pma)
      where
        replaceSubVector :: Int -> Vector Int -> Vector Int -> Vector Int
        replaceSubVector startPos newSubVector origVector =
          (Vector.take startPos origVector) <> newSubVector <> (Vector.drop (startPos + (Vector.length newSubVector)) origVector)

    newElements = replaceSubVector (windowStart * (segmentCapacity pma)) newSubElements (elements pma)
      where
        replaceSubVector :: Int -> Vector (Maybe a) -> Vector (Maybe a) -> Vector (Maybe a)
        replaceSubVector startPos newSubVector origVector =
          (Vector.take startPos origVector) <> newSubVector <> (Vector.drop (startPos + (Vector.length newSubVector)) origVector)


rebalance :: PMA k v -> Int -> PMA k v
rebalance pma segmentId = rebalancedPMA
  where
    (newDensity, t_l, elementsCnt, windowStart, windowLength) =
      if ((height pma) > 1)
        then (tryHigher 2 2 (segmentId `div` 2))
        else (1.0, t_h, ((segmentsCardinalities pma) Vector.! segmentId), segmentId, 1)
      where
        tryHigher :: Int -> Int -> Int -> (Double, Double, Int, Int, Int)
        tryHigher l windowLength windowId = if ((density >= t_l) && l < (height pma)) then tryHigher (l + 1) (windowLength * 2) (windowId `div` 2) else (density, t_l, elementsCnt, windowStart, windowLength)
          where
            windowStart = windowId * windowLength --inclusive
            windowEnd = windowStart + windowLength --exclusive
            (p_l, t_l) = windowThresholds pma l
            -- todo rewrite more optimised...
            elementsCnt = sum (Vector.take windowLength (Vector.drop windowStart (segmentsCardinalities pma)))
            
            density = (fromIntegral elementsCnt) / (fromIntegral (windowLength * (segmentCapacity pma)))

    rebalancedPMA = if (newDensity >= t_l) 
      then resize pma
      else spread pma elementsCnt windowStart windowLength



findSegment :: forall k v. Ord k => PMA k v -> k -> Int
findSegment pma val = if (Data.PackedMemoryArray.null pma) then 0 else (find pma 0 ((segmentsCnt pma) - 1))
  where
    find :: PMA k v -> Int -> Int -> Int
    find pma lb ub = if (lb < ub) then (find pma newLB newUB) else lb
      where
        mid = (lb + ub) `div` 2
        (newLB, newUB) =  if ((Just val) < fmap fst ((elements pma) Vector.! (mid * (segmentCapacity pma))))
                        then (lb, mid - 1)
                        else  if ((Just val) <= fmap fst ((elements pma) Vector.! ((mid * (segmentCapacity pma)) + ((segmentsCardinalities pma) Vector.! mid) - 1)))
                            then (mid, mid)
                            else (mid + 1, ub)

-- todo write tests for this
-- | finds index where an element should be put
-- complexity – O (log (n / log n) + log n) = O (2 log (n) - log log n)
-- replace linear search to binary – get O (log (n / log n) + log log n) = O (log n)
findPlaceIndex :: Ord k => k -> PMA k a -> Int
findPlaceIndex val pma = start + linSearch (Vector.slice start len ((fmap fst) <$> elements pma))
  where
    segmentId = findSegment pma val
    start = segmentId * segmentCapacity pma
    len = segmentCapacity pma
    
    linSearch slice = fromMaybe
        (length slice - 1)
        (Vector.findIndex (>= Just val) slice)

-- * Tests

pairWithShow :: Show a => a -> (a, String)
pairWithShow a = (a, show a)

samplePMA_1 :: PMA Int String
samplePMA_1 = PMA
  { capacity = 8
  , segmentCapacity = 4
  , height = 2
  , elements = (fmap pairWithShow) <$> Vector.fromList
      [ Just 1, Just 2, Just 3, Just 4
      , Nothing, Nothing, Nothing, Nothing
      ]
  , cardinality = 4
  , segmentsCnt = 2
  , segmentsCardinalities = Vector.fromList [4, 0]
  }

samplePMA_2 :: PMA Int String
samplePMA_2 = PMA
  { capacity = 8,
    segmentCapacity = 4,
    height = 2,
    elements = (fmap pairWithShow) <$> Vector.fromList
      [ Just 1, Just 2, Nothing, Nothing
      , Just 3, Just 4, Nothing, Nothing
      ]
    , cardinality = 4
    , segmentsCnt = 2
    , segmentsCardinalities = Vector.fromList [2,2]
  }

samplePMA_3 :: PMA Int String
samplePMA_3 = PMA
  { capacity = 8,
    segmentCapacity = 8,
    height = 1,
    elements = (fmap pairWithShow) <$> Vector.fromList
      [ Just 1, Just 2, Just 3, Just 4
      , Just 5, Just 6, Just 7, Nothing
      ]
    , cardinality = 7
    , segmentsCnt = 1
    , segmentsCardinalities = Vector.fromList [7]
  }

samplePMA_4 :: PMA Int String
samplePMA_4 = PMA
  { capacity = 16
  , segmentCapacity = 4
  , height = 3
  , elements = (fmap pairWithShow) <$> Vector.fromList
      [ Just 1,Just 2,Nothing,Nothing
      , Just 4,Just 5,Nothing,Nothing
      , Just 6,Just 7,Nothing,Nothing
      , Just 8,Just 9,Nothing,Nothing
      ]
  , cardinality = 8
  , segmentsCnt = 4
  , segmentsCardinalities = Vector.fromList [2,2,2,2]
}
