{-# OPTIONS_HADDOCK not-home #-}

module Hedgehog.Internal.Discovery
  ( readDeclaration,
    Pos (..),
    Position (..),
  )
where

import Control.Exception (IOException, handle)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord qualified as Ord
import Hedgehog.Internal.Source (ColumnNo (..), LineNo (..))

------------------------------------------------------------------------
-- Property Extraction

readDeclaration :: FilePath -> LineNo -> IO (Maybe (String, Pos String))
readDeclaration path line = do
  mfile <- readFileSafe path
  pure do
    file <- mfile
    takeHead
      . List.sortBy (Ord.comparing $ Ord.Down . posLine . posPostion . snd)
      . filter ((<= line) . posLine . posPostion . snd)
      $ Map.toList (findDeclarations path file)

readFileSafe :: FilePath -> IO (Maybe String)
readFileSafe path =
  handle (\(_ :: IOException) -> pure Nothing) (Just <$> readFile path)

takeHead :: [a] -> Maybe a
takeHead = \case
  [] -> Nothing
  x : _ -> Just x

findDeclarations :: FilePath -> String -> Map String (Pos String)
findDeclarations path =
  declarations . classified . positioned path

------------------------------------------------------------------------
-- Declaration Identification

declarations :: [Classified (Pos Char)] -> Map String (Pos String)
declarations =
  let loop = \case
        [] -> []
        x : xs ->
          let (ys, zs) = break isDeclaration xs
           in tagWithName (forget x $ trimEnd ys) : loop zs
   in Map.fromListWith (<>) . loop . dropWhile (not . isDeclaration)

trimEnd :: [Classified (Pos Char)] -> [Classified (Pos Char)]
trimEnd xs =
  let (space0, code) = span isWhitespace (reverse xs)
      (line_tail0, space) = span (\(Classified _ (Pos _ x)) -> x /= '\n') (reverse space0)
      line_tail =
        case space of
          [] -> line_tail0
          x : _ -> line_tail0 ++ [x]
   in reverse code ++ line_tail

isWhitespace :: Classified (Pos Char) -> Bool
isWhitespace (Classified c (Pos _ x)) =
  c == Comment || Char.isSpace x

tagWithName :: Pos String -> (String, Pos String)
tagWithName (Pos p x) =
  (takeName x, Pos p x)

takeName :: String -> String
takeName xs =
  case words xs of
    [] -> ""
    x : _ -> x

forget :: Classified (Pos Char) -> [Classified (Pos Char)] -> Pos String
forget (Classified _ (Pos p x)) xs =
  Pos p (x : fmap (posValue . classifiedValue) xs)

isDeclaration :: Classified (Pos Char) -> Bool
isDeclaration (Classified c (Pos p x)) =
  and
    [ c == NotComment,
      posColumn p == 1,
      Char.isLower x || x == '_'
    ]

------------------------------------------------------------------------
-- Comment Classification

data Class
  = NotComment
  | Comment
  deriving (Eq, Ord, Show)

data Classified a = Classified
  { _classifiedClass :: !Class,
    classifiedValue :: !a
  }
  deriving (Eq, Ord, Show)

classified :: [Pos Char] -> [Classified (Pos Char)]
classified =
  let ok = Classified NotComment
      ko = Classified Comment

      loop nesting in_line = \case
        [] -> []
        x@(Pos _ '\n') : xs | in_line -> ok x : loop nesting False xs
        x : xs | in_line -> ko x : loop nesting in_line xs
        x@(Pos _ '{') : y@(Pos _ '-') : xs -> ko x : ko y : loop (nesting + 1) in_line xs
        x@(Pos _ '-') : y@(Pos _ '}') : xs | nesting > 0 -> ko x : ko y : loop (nesting - 1) in_line xs
        x : xs | nesting > 0 -> ko x : loop nesting in_line xs
        -- FIXME This is not technically correct, we should allow arbitrary runs
        -- FIXME of dashes followed by a symbol character. Here we have only
        -- FIXME allowed two.
        x@(Pos _ '-') : y@(Pos _ '-') : z@(Pos _ zz) : xs
          | not (Char.isSymbol zz) ->
              ko x : ko y : loop nesting True (z : xs)
        x : xs -> ok x : loop nesting in_line xs
   in loop (0 :: Int) False

------------------------------------------------------------------------
-- Character Positioning

data Position = Position
  { _posPath :: !FilePath,
    posLine :: !LineNo,
    posColumn :: !ColumnNo
  }
  deriving (Eq, Ord, Show)

data Pos a = Pos
  { posPostion :: !Position,
    posValue :: a
  }
  deriving (Eq, Ord, Show, Functor)

instance (Semigroup a) => Semigroup (Pos a) where
  Pos p x <> Pos q y
    | p < q = Pos p (x <> y)
    | otherwise = Pos q (y <> x)

positioned :: FilePath -> [Char] -> [Pos Char]
positioned path =
  let loop l c = \case
        [] -> []
        '\n' : xs -> Pos (Position path l c) '\n' : loop (l + 1) 1 xs
        x : xs -> Pos (Position path l c) x : loop l (c + 1) xs
   in loop 1 1
