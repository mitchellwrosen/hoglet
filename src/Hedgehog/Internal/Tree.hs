{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Hedgehog.Internal.Tree (
    Tree(..)
  , singleton
  , zipTreeT
  , treeValue

  , NodeT(..)
  , pattern Node
  , fromNodeT

  , expand
  , prune

  , consChild
  , mapMaybeT
  , interleaveTreeT

  , render
  ) where

#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative (liftA2)
#endif

import           Data.Functor.Identity (Identity(..))
import           Data.Functor.Classes (Show1(..), showsPrec1)
import           Data.Functor.Classes (showsUnaryWith, showsBinaryWith)
import qualified Data.List as List

import           Prelude hiding (filter)

------------------------------------------------------------------------

-- | A rose tree.
--
newtype Tree a =
  Tree {
      runTree :: NodeT Identity a
    }
  deriving stock (Eq, Functor)

-- | A node in a rose tree.
--
type Node =
  NodeT Identity
{-# COMPLETE Node #-}

-- | Pattern to ease construction / deconstruction of pure nodes.
--
pattern Node :: a -> [Tree a] -> Node a
pattern Node x xs =
  NodeT x xs

-- | A node in an effectful tree, as well as its unevaluated children.
--
data NodeT m a =
  NodeT {
      -- | The value at this 'NodeT' in the 'TreeT'.
      nodeValue :: a

      -- | The children of this 'NodeT'.
    , nodeChildren :: [Tree a]
    } deriving (Eq)

-- | Create a 'TreeT' from a 'NodeT'
--
fromNodeT :: NodeT Identity a -> Tree a
fromNodeT =
  Tree

singleton :: a -> Tree a
singleton =
  pure

-- | The value at the root of the 'Tree'.
--
treeValue :: Tree a -> a
treeValue =
  nodeValue . runTree

-- | Create a tree from a value and an unfolding function.
--
unfold :: (a -> [a]) -> a -> Tree a
unfold f x =
  Tree $
    NodeT x (unfoldForest f x)

-- | Create a forest from a value and an unfolding function.
--
unfoldForest :: (a -> [a]) -> a -> [Tree a]
unfoldForest f =
  fmap (unfold f) . f

-- | Expand a tree using an unfolding function.
--
expand :: (a -> [a]) -> Tree a -> Tree a
expand f (Tree (Node x xs)) =
  Tree (Node x (map (expand f) xs ++ unfoldForest f x))

-- | Throw away all but the top @n@ levels of a tree's children.
--
--   /@prune 0@ will throw away all of a tree's children./
--
prune :: Int -> Tree a -> Tree a
prune n (Tree (Node x xs)) =
  if n <= 0 then
    fromNodeT (Node x [])
  else
    fromNodeT (Node x (map (prune (n - 1)) xs))

mapMaybeT :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeT p (Tree (Node x xs)) =
  (\y -> fromNodeT (Node y (flattenTrees p xs))) <$> p x

flattenTree :: (a -> Maybe b) -> Tree a -> [Tree b]
flattenTree p (Tree (Node x ys0)) =
  let
    ys =
      flattenTrees p ys0
  in
    case p x of
      Just x' ->
        [Tree (Node x' ys)]
      Nothing ->
        ys

flattenTrees :: (a -> Maybe b) -> [Tree a] -> [Tree b]
flattenTrees =
  concatMap .
  flattenTree

consChild :: a -> Tree a -> Tree a
consChild a m =
  Tree $
    let NodeT x xs = runTree m
    in NodeT x $ singleton a : xs

------------------------------------------------------------------------

-- | All ways a list can be split
--
-- > splits [1,2,3]
-- > ==
-- > [ ([], 1, [2, 3])
--   , ([1], 2, [3])
--   , ([1, 2], 3, [])
--   ]
--
splits :: [a] -> [([a], a, [a])]
splits xs0 =
  let
    go (front : fronts) (x : xs) =
      (front, x, xs) : go fronts xs
    go _ _ =
      []
  in
    go (List.inits xs0) xs0

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

dropSome :: [NodeT Identity a] -> [Tree [a]]
dropSome ts = do
  n   <- takeWhile (> 0) $ iterate (`div` 2) (length ts)
  ts' <- removes n ts
  [fromNodeT $ interleave ts']

shrinkOne :: [NodeT Identity a] -> [Tree [a]]
shrinkOne ts = do
  (xs, y0, zs) <- splits ts
  y1 <- nodeChildren y0
  [fromNodeT $ interleave (xs ++ [runTree y1] ++ zs)]

interleave :: [NodeT Identity a] -> NodeT Identity [a]
interleave ts =
  NodeT (fmap nodeValue ts) $
    concat [
        dropSome ts
      , shrinkOne ts
      ]

interleaveTreeT :: [Tree a] -> NodeT Identity [a]
interleaveTreeT =
  interleave . map runTree

------------------------------------------------------------------------

instance Foldable Tree where
  foldMap f (Tree mx) =
    foldMap f mx

instance Foldable Node where
  foldMap f (NodeT x xs) =
    f x `mappend` mconcat (fmap (foldMap f) xs)

instance Traversable Tree where
  traverse f (Tree mx) =
    Tree <$> traverse f mx

instance Traversable Node where
  traverse f (NodeT x xs) =
    NodeT <$> f x <*> traverse (traverse f) xs

------------------------------------------------------------------------
-- NodeT/TreeT instances

instance Functor m => Functor (NodeT m) where
  fmap f (NodeT x xs) =
    NodeT (f x) (fmap (fmap f) xs)

instance Applicative (NodeT Identity) where
  pure x =
    NodeT x []
  (<*>) (NodeT ab tabs) na@(NodeT a tas) =
    NodeT (ab a) $
      map (<*> (fromNodeT na)) tabs ++ map (fmap ab) tas

instance Applicative Tree where
  pure =
    Tree . pure
  (<*>) (Tree mab) (Tree ma) =
    Tree $
      (<*>) mab ma

instance Monad (NodeT Identity) where
  return =
    pure

  (>>=) (NodeT x xs) k =
    case k x of
      NodeT y ys ->
        NodeT y $
          map (Tree . (>>= k) . runTree) xs ++ ys

instance Monad Tree where
  return =
    pure

  (>>=) m k =
    Tree $ do
      let NodeT x xs = runTree m
      let NodeT y ys = runTree (k x)
      NodeT y $
        map (>>= k) xs ++ ys

zipTreeT :: forall a b. Tree a -> Tree b -> Tree (a, b)
zipTreeT l0@(Tree left) r0@(Tree right) =
  Tree $
    let
      zipNodeT :: NodeT Identity a -> NodeT Identity b -> NodeT Identity (a, b)
      zipNodeT (NodeT a ls) (NodeT b rs) =
          NodeT (a, b) $
            concat [
                [zipTreeT l1 r0 | l1 <- ls]
              , [zipTreeT l0 r1 | r1 <- rs]
              ]
    in
      zipNodeT left right

------------------------------------------------------------------------
-- Show/Show1 instances

instance (Show1 m, Show a) => Show (NodeT m a) where
  showsPrec =
    showsPrec1

instance (Show a) => Show (Tree a) where
  showsPrec =
    showsPrec1

instance Show1 m => Show1 (NodeT m) where
  liftShowsPrec sp sl d (NodeT x xs) =
    let
      sp1 =
        liftShowsPrec sp sl

      sl1 =
        liftShowList sp sl

      sp2 =
        liftShowsPrec sp1 sl1
    in
      showsBinaryWith sp sp2 "NodeT" d x xs

instance Show1 Tree where
  liftShowsPrec sp sl d (Tree m) =
    let
      sp1 =
        liftShowsPrec sp sl

      sl1 =
        liftShowList sp sl

      sp2 =
        liftShowsPrec sp1 sl1
    in
      showsUnaryWith sp2 "TreeT" d (Identity m)

------------------------------------------------------------------------
-- Pretty Printing

--
-- Rendering implementation based on the one from containers/Data.Tree
--

renderTreeTLines :: Tree String -> [String]
renderTreeTLines (Tree (Node x xs0)) = do
  lines (renderNodeT x) ++ renderForestLines xs0

renderNodeT :: String -> String
renderNodeT xs =
  case xs of
    [_] ->
      ' ' : xs
    _ ->
      xs

renderForestLines :: [Tree String] -> [String]
renderForestLines xs0 =
  let
    shift hd other =
      zipWith (++) (hd : repeat other)
  in
    case xs0 of
      [] ->
        []

      [x] ->
        shift " └╼" "   " (renderTreeTLines x)

      x : xs ->
        shift " ├╼" " │ " (renderTreeTLines x) ++ (renderForestLines xs)

-- | Render a tree of strings.
--
render :: Tree String -> String
render =
  unlines . renderTreeTLines
