{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
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
    Tree
  , pattern Tree
  , TreeT(..)
  , zipTreeT
  , treeValue

  , NodeT(..)
  , fromNodeT

  , expand
  , prune

  , catMaybes
  , runTreeMaybeT
  , mapMaybeMaybeT
  , consChild
  , mapMaybeT
  , interleave
  , interleaveTreeT

  , render
  ) where

#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative (liftA2)
#endif
import           Control.Applicative (Alternative(..))
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Trans.Class (MonadTrans(..))

import           Data.Functor.Identity (Identity(..))
import           Data.Functor.Classes (Eq1(..))
import           Data.Functor.Classes (Show1(..), showsPrec1)
import           Data.Functor.Classes (showsUnaryWith, showsBinaryWith)
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import           Prelude hiding (filter)

------------------------------------------------------------------------

-- | A rose tree.
--
type Tree =
  TreeT Identity

-- | Pattern to ease construction / deconstruction of pure trees.
--
pattern Tree :: NodeT Identity a -> Tree a
pattern Tree node =
  TreeT (Identity node)
{-# COMPLETE Tree #-}

-- | An effectful tree, each node in the tree can have an effect before it is
--   produced.
--
newtype TreeT m a =
  TreeT {
      runTreeT :: m (NodeT m a)
    }

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
    , nodeChildren :: [TreeT m a]
    } deriving (Eq)

-- | Extracts the 'Node' from a 'Tree'.
--
runTree :: Tree a -> Node a
runTree =
  runIdentity . runTreeT

-- | Create a 'TreeT' from a 'NodeT'
--
fromNodeT :: Applicative m => NodeT m a -> TreeT m a
fromNodeT =
  TreeT . pure

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
unfold :: (a -> [a]) -> a -> TreeT Maybe a
unfold f x =
  TreeT . Just $
    NodeT x (unfoldForest f x)

-- | Create a forest from a value and an unfolding function.
--
unfoldForest :: (a -> [a]) -> a -> [TreeT Maybe a]
unfoldForest f =
  fmap (unfold f) . f

-- | Expand a tree using an unfolding function.
--
expand :: (a -> [a]) -> TreeT Maybe a -> TreeT Maybe a
expand f m =
  TreeT $ do
    NodeT x xs <- runTreeT m
    Just . NodeT x $
      fmap (expand f) xs ++ unfoldForest f x

-- | Throw away all but the top @n@ levels of a tree's children.
--
--   /@prune 0@ will throw away all of a tree's children./
--
prune :: Int -> TreeT Maybe a -> TreeT Maybe a
prune n m =
  if n <= 0 then
    TreeT $ do
      NodeT x _ <- runTreeT m
      Just $ NodeT x []
  else
    TreeT $ do
      NodeT x xs0 <- runTreeT m
      Just . NodeT x $
        fmap (prune (n - 1)) xs0

-- | Takes a tree of 'Maybe's and returns a tree of all the 'Just' values.
--
--   If there are no 'Just' values then 'Nothing' is returned.
--
catMaybes :: Tree (Maybe a) -> Maybe (Tree a)
catMaybes m =
  let
    NodeT mx mxs =
      runTree m
  in
    case mx of
      Nothing -> do
        case Maybe.mapMaybe catMaybes mxs of
          [] ->
            Nothing
          Tree (NodeT x xs0) : xs1 ->
            Just . Tree $
              Node x (xs0 ++ xs1)
      Just x ->
        Just . Tree $
          Node x (Maybe.mapMaybe catMaybes mxs)

-- | Run the discard effects through the tree and reify them as 'Maybe' values
--   at the nodes.
--
--   'Nothing' means discarded, 'Just' means we have a value.
--
runTreeMaybeT :: TreeT Maybe a -> Tree (Maybe a)
runTreeMaybeT tree =
  case runTreeT tree of
    Nothing -> singleton Nothing
    Just (NodeT root children) -> fromNodeT (NodeT (Just root) (map runTreeMaybeT children))

runTreeMaybeT2 :: TreeT Maybe a -> Maybe (Tree a)
runTreeMaybeT2 tree =
  case runTreeT tree of
    Nothing -> Nothing
    Just (NodeT root children) -> Just (fromNodeT (NodeT root (Maybe.mapMaybe runTreeMaybeT2 children)))

mapMaybeMaybeT :: (a -> Maybe b) -> TreeT Maybe a -> TreeT Maybe b
mapMaybeMaybeT p t =
  case runTreeMaybeT2 t of
    Nothing ->
      TreeT Nothing
    Just (Tree (Node x xs)) ->
      case p x of
        Nothing -> TreeT Nothing
        Just x' ->
          hoist (Just . runIdentity) $
            Tree . Node x' $
              concatMap (flattenTree p) xs

flattenTree :: (a -> Maybe b) -> Tree a -> [Tree b]
flattenTree p (Tree (Node x ys0)) =
  let
    ys =
      concatMap (flattenTree p) ys0
  in
    case p x of
      Just x' ->
        [Tree (Node x' ys)]
      Nothing ->
        ys

mapMaybeT :: (a -> Maybe b) -> TreeT Maybe a -> TreeT Maybe b
mapMaybeT p m =
  TreeT $ do
    NodeT x xs <- runTreeT m
    (\x' -> NodeT x' (fmap (mapMaybeT p) xs)) <$> p x

consChild :: a -> Tree a -> Tree a
consChild a m =
  TreeT $ do
    NodeT x xs <- runTreeT m
    Identity $
      NodeT x $
        singleton a : xs

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

dropSome :: [NodeT Maybe a] -> [TreeT Maybe [a]]
dropSome ts = do
  n   <- takeWhile (> 0) $ iterate (`div` 2) (length ts)
  ts' <- removes n ts
  [TreeT . Just $ interleave ts']

shrinkOne :: [NodeT Maybe a] -> [TreeT Maybe [a]]
shrinkOne ts = do
  (xs, y0, zs) <- splits ts
  y1 <- nodeChildren y0
  [TreeT $ do
    y2 <- runTreeT y1
    Just $ interleave (xs ++ [y2] ++ zs)]

interleave :: [NodeT Maybe a] -> NodeT Maybe [a]
interleave ts =
  NodeT (fmap nodeValue ts) $
    concat [
        dropSome ts
      , shrinkOne ts
      ]

interleaveTreeT :: [TreeT Maybe a] -> Maybe (NodeT Maybe [a])
interleaveTreeT =
  fmap interleave . traverse runTreeT

------------------------------------------------------------------------

instance Foldable Tree where
  foldMap f (TreeT mx) =
    foldMap f (runIdentity mx)

instance Foldable Node where
  foldMap f (NodeT x xs) =
    f x `mappend` mconcat (fmap (foldMap f) xs)

instance Traversable Tree where
  traverse f (TreeT mx) =
    TreeT <$> traverse (traverse f) mx

instance Traversable Node where
  traverse f (NodeT x xs) =
    NodeT <$> f x <*> traverse (traverse f) xs

------------------------------------------------------------------------
-- NodeT/TreeT instances

instance (Eq1 m, Eq a) => Eq (TreeT m a) where
  TreeT m0 == TreeT m1 =
    liftEq (==) m0 m1

instance Functor m => Functor (NodeT m) where
  fmap f (NodeT x xs) =
    NodeT (f x) (fmap (fmap f) xs)

instance Functor m => Functor (TreeT m) where
  fmap f =
    TreeT . fmap (fmap f) . runTreeT

instance Applicative m => Applicative (NodeT m) where
  pure x =
    NodeT x []
  (<*>) (NodeT ab tabs) na@(NodeT a tas) =
    NodeT (ab a) $
      map (<*> (fromNodeT na)) tabs ++ map (fmap ab) tas

instance Applicative m => Applicative (TreeT m) where
  pure =
    TreeT . pure . pure
  (<*>) (TreeT mab) (TreeT ma) =
    TreeT $
      liftA2 (<*>) mab ma

instance Monad m => Monad (NodeT m) where
  return =
    pure

  (>>=) (NodeT x xs) k =
    case k x of
      NodeT y ys ->
        NodeT y $
          fmap (TreeT . fmap (>>= k) . runTreeT) xs ++ ys

instance Monad m => Monad (TreeT m) where
  return =
    pure

  (>>=) m k =
    TreeT $ do
      NodeT x xs <- runTreeT m
      NodeT y ys <- runTreeT (k x)
      pure . NodeT y $
        fmap (>>= k) xs ++ ys

instance Alternative m => Alternative (TreeT m) where
  empty =
    TreeT empty
  (<|>) x y =
    TreeT (runTreeT x <|> runTreeT y)

zipTreeT :: forall a b. TreeT Maybe a -> TreeT Maybe b -> TreeT Maybe (a, b)
zipTreeT l0@(TreeT left) r0@(TreeT right) =
  TreeT $
    let
      zipNodeT :: NodeT Maybe a -> NodeT Maybe b -> NodeT Maybe (a, b)
      zipNodeT (NodeT a ls) (NodeT b rs) =
          NodeT (a, b) $
            concat [
                [zipTreeT l1 r0 | l1 <- ls]
              , [zipTreeT l0 r1 | r1 <- rs]
              ]
    in
      zipNodeT <$> left <*> right

instance MonadTrans TreeT where
  lift f =
    TreeT $
      fmap (\x -> NodeT x []) f

instance MFunctor NodeT where
  hoist f (NodeT x xs) =
    NodeT x (fmap (hoist f) xs)

instance MFunctor TreeT where
  hoist f (TreeT m) =
    TreeT . f $ fmap (hoist f) m

------------------------------------------------------------------------
-- Show/Show1 instances

instance (Show1 m, Show a) => Show (NodeT m a) where
  showsPrec =
    showsPrec1

instance (Show1 m, Show a) => Show (TreeT m a) where
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

instance Show1 m => Show1 (TreeT m) where
  liftShowsPrec sp sl d (TreeT m) =
    let
      sp1 =
        liftShowsPrec sp sl

      sl1 =
        liftShowList sp sl

      sp2 =
        liftShowsPrec sp1 sl1
    in
      showsUnaryWith sp2 "TreeT" d m

------------------------------------------------------------------------
-- Pretty Printing

--
-- Rendering implementation based on the one from containers/Data.Tree
--

renderTreeTLines :: TreeT Identity String -> [String]
renderTreeTLines (Tree (Node x xs0)) = do
  lines (renderNodeT x) ++ renderForestLines xs0

renderNodeT :: String -> String
renderNodeT xs =
  case xs of
    [_] ->
      ' ' : xs
    _ ->
      xs

renderForestLines :: [TreeT Identity String] -> [String]
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
