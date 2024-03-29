{-# OPTIONS_HADDOCK not-home #-}

module Hedgehog.Internal.Tree
  ( Tree (..),
    singleton,
    zipTreeT,
    treeValue,
    expand,
    prune,
    consChild,
    mapMaybe,
    interleave,
    render,
  )
where

import Data.Functor.Classes (Show1 (..), showsBinaryWith, showsPrec1)
import Data.List qualified as List
import Prelude hiding (filter)

------------------------------------------------------------------------

-- | A rose tree.
data Tree a
  = Tree a [Tree a]
  deriving stock (Eq, Foldable, Functor, Traversable)

treeValue :: Tree a -> a
treeValue (Tree value _) =
  value

treeChildren :: Tree a -> [Tree a]
treeChildren (Tree _ children) =
  children

singleton :: a -> Tree a
singleton =
  pure

-- | Create a tree from a value and an unfolding function.
unfold :: (a -> [a]) -> a -> Tree a
unfold f x =
  Tree x (unfoldForest f x)

-- | Create a forest from a value and an unfolding function.
unfoldForest :: (a -> [a]) -> a -> [Tree a]
unfoldForest f =
  fmap (unfold f) . f

-- | Expand a tree using an unfolding function.
expand :: (a -> [a]) -> Tree a -> Tree a
expand f (Tree x xs) =
  Tree x (map (expand f) xs ++ unfoldForest f x)

-- | Throw away all but the top @n@ levels of a tree's children.
--
--   /@prune 0@ will throw away all of a tree's children./
prune :: Int -> Tree a -> Tree a
prune n (Tree x xs)
  | n <= 0 = Tree x []
  | otherwise = Tree x (map (prune (n - 1)) xs)

mapMaybe :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybe p (Tree x xs) =
  Tree <$> p x <*> Just (flattenForest p xs)

flattenTree :: (a -> Maybe b) -> Tree a -> [Tree b]
flattenTree p (Tree x ys0) =
  let ys = flattenForest p ys0
   in case p x of
        Just x' -> [Tree x' ys]
        Nothing -> ys

flattenForest :: (a -> Maybe b) -> [Tree a] -> [Tree b]
flattenForest =
  concatMap . flattenTree

consChild :: a -> Tree a -> Tree a
consChild a (Tree x xs) =
  Tree x (singleton a : xs)

------------------------------------------------------------------------

-- | All ways a list can be split
--
-- > splits [1,2,3]
-- > ==
-- > [ ([], 1, [2, 3])
--   , ([1], 2, [3])
--   , ([1, 2], 3, [])
--   ]
splits :: [a] -> [([a], a, [a])]
splits xs0 =
  go (List.inits xs0) xs0
  where
    go (front : fronts) (x : xs) = (front, x, xs) : go fronts xs
    go _ _ = []

-- | @removes n@ computes all ways we can remove chunks of size @n@ from a list
--
-- Examples
--
-- > removes 1 [1..3] == [[2,3],[1,3],[1,2]]
-- > removes 2 [1..4] == [[3,4],[1,2]]
-- > removes 2 [1..5] == [[3,4,5],[1,2,5],[1,2,3,4]]
-- > removes 3 [1..5] == [[4,5],[1,2,3]]
--
-- Note that the last chunk we delete might have fewer elements than @n@.
removes :: forall a. Int -> [a] -> [[a]]
removes k = \xs -> go xs
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = xs2 : map (xs1 ++) (go xs2)
      where
        (xs1, xs2) = splitAt k xs

dropSome :: [Tree a] -> [Tree [a]]
dropSome ts = do
  n <- takeWhile (> 0) $ iterate (`div` 2) (length ts)
  ts' <- removes n ts
  [interleave ts']

shrinkOne :: [Tree a] -> [Tree [a]]
shrinkOne ts = do
  (xs, y0, zs) <- splits ts
  y1 <- treeChildren y0
  [interleave (xs ++ [y1] ++ zs)]

interleave :: [Tree a] -> Tree [a]
interleave xs =
  Tree (map treeValue xs) (dropSome xs ++ shrinkOne xs)

------------------------------------------------------------------------
-- NodeT/TreeT instances

instance Applicative Tree where
  pure x =
    Tree x []
  Tree ab tabs <*> na@(Tree a tas) =
    Tree (ab a) (map (<*> na) tabs ++ map (fmap ab) tas)

instance Monad Tree where
  return =
    pure

  Tree x xs >>= k =
    let Tree y ys = k x
     in Tree y (map (>>= k) xs ++ ys)

zipTreeT :: forall a b. Tree a -> Tree b -> Tree (a, b)
zipTreeT x y =
  Tree (treeValue x, treeValue y) (xs ++ ys)
  where
    xs = [zipTreeT x' y | x' <- treeChildren x]
    ys = [zipTreeT x y' | y' <- treeChildren y]

------------------------------------------------------------------------
-- Show/Show1 instances

instance (Show a) => Show (Tree a) where
  showsPrec =
    showsPrec1

instance Show1 Tree where
  liftShowsPrec sp sl d (Tree x xs) =
    showsBinaryWith sp (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)) "Tree" d x xs

------------------------------------------------------------------------
-- Pretty Printing

--
-- Rendering implementation based on the one from containers/Data.Tree
--

renderTreeTLines :: Tree String -> [String]
renderTreeTLines (Tree x xs0) = do
  lines (renderNodeT x) ++ renderForestLines xs0

renderNodeT :: String -> String
renderNodeT xs =
  case xs of
    [_] -> ' ' : xs
    _ -> xs

renderForestLines :: [Tree String] -> [String]
renderForestLines xs0 =
  let shift hd other = zipWith (++) (hd : repeat other)
   in case xs0 of
        [] -> []
        [x] -> shift " └╼" "   " (renderTreeTLines x)
        x : xs -> shift " ├╼" " │ " (renderTreeTLines x) ++ (renderForestLines xs)

-- | Render a tree of strings.
render :: Tree String -> String
render =
  unlines . renderTreeTLines
