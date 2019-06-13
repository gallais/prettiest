{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, ViewPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase, PatternGuards, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Text.PrettyPrint.Compact.Core
       ( Annotation
       , Layout(..)
       , Renderable(..)
       , Document
       , Options(..)
       , groupingBy
       , ODoc
       , Doc
       , Doc'
       , LM
       , M
       , Pair
       , ($$)
       ) where

import Prelude ()
import Prelude.Compat as P

import Data.List.Compat (sortOn,groupBy,minimumBy)
import Data.Function (on)
import Data.Semigroup
import Data.Sequence (singleton, Seq, viewl, viewr, ViewL(..), ViewR(..), (|>))
import Data.String
import Data.Foldable (toList)
import Control.Applicative (liftA2)
-- | Annotated string, which consists of segments with separate (or no) annotations.
--
-- We keep annotated segments in a container (list).
-- The annotation is @Maybe a@, because the no-annotation case is common.
--
-- /Note:/ with @Last x@ annotation, the 'annotate' will overwrite all annotations.
--
-- /Note:/ if the list is changed into `Seq` or similar structure
-- allowing fast viewr and viewl, then we can impose an additional
-- invariant that there aren't two consequtive non-annotated segments;
-- yet there is no performance reason to do so.
--
data AS a = AS {-# UNPACK #-} !Int [(a, String)]
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

-- | Tests the invariants of 'AS'
_validAs :: AS a -> Bool
_validAs (AS i s) = lengthInvariant && noNewlineInvariant
  where
    lengthInvariant = i == sum (map (length . snd) s)
    noNewlineInvariant = all (notElem '\n' . snd) s

asLength :: AS a -> Int
asLength (AS l _) = l

-- | Make a non-annotated 'AS'.
mkAS :: Monoid a => String -> AS a
mkAS s = AS (length s) [(mempty, s)]

instance Semigroup (AS a) where
  AS i xs <> AS j ys = AS (i + j) (xs <> ys)

newtype L a = L (Seq (AS a)) -- non-empty sequence
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance Monoid a => Semigroup (L a) where
  L (viewr -> xs :> x) <> L (viewl -> y :< ys) = L (xs <> singleton (x <> y) <> fmap (indent <>) ys)
      where n = asLength x
            indent = mkAS (P.replicate n ' ')
  L _ <> L _ = error "<> @L: invariant violated, Seq is empty"

instance Monoid a => Monoid (L a) where
   mempty = L (singleton (mkAS ""))
   mappend = (<>)

annotateAS :: Semigroup a => a -> AS a -> AS a
annotateAS a = fmap (a <>)

instance Layout L where
   text = L . singleton . mkAS
   flush (L xs) = L (xs |> mkAS "")
   annotate a (L s') = L (fmap (annotateAS a) s')

renderWithList :: (Monoid a, Monoid r) => Options a r -> [AS a] -> r
renderWithList opts xs = intercalate xs
  where
    f = optsAnnotate opts
    f' (AS _ s) = foldMap (uncurry f) s
    sep = f mempty "\n"

    intercalate []     = mempty
    intercalate (y:ys) = f' y `mappend` foldMap (mappend sep . f') ys

{-# INLINE renderWithL #-}
renderWithL :: (Monoid a, Monoid r) => Options a r -> L a -> r
renderWithL opts (L xs) = renderWithList opts (toList xs)

{-# INLINE renderWithLM #-}
renderWithLM :: (Monoid a, Monoid r) => Options a r -> LM a -> r
renderWithLM opts x = renderWithList opts (lmToList x)

{-# INLINE renderWithL' #-}
renderWithL' :: (Monoid a, Monoid r) => Options a r -> L' a -> r
renderWithL' opts x = renderWithList opts (lToList x)

data Options a r = Options
    { optsPageWidth :: !Int              -- ^ maximum page width
    , optsAnnotate  :: a -> String -> r  -- ^ how to annotate the string. /Note:/ the annotation should preserve the visible length of the string.
    }

class Layout d where
  text :: Monoid a => String -> d a
  flush :: Monoid a => d a -> d a
  -- | `<>` new annotation to the 'Doc'.
  --
  -- Example: 'Any True' annotation will transform the rendered 'Doc' into uppercase:
  --
  -- >>> let r = putStrLn . renderWith defaultOptions { optsAnnotate = \a x -> if a == Any True then map toUpper x else x }
  -- >>> r $ text "hello" <$$> annotate (Any True) (text "world")
  -- hello
  -- WORLD
  --
  annotate :: forall a. Monoid a => a -> d a -> d a

-- type parameter is phantom.
data M a = M
  { height    :: !Int
  , lastWidth :: !Int
  , maxWidth  :: !Int
  } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

data Tree a = Leaf | Node !(Tree a) a !(Tree a)
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

data L' a = L'
  { _block' :: Maybe (AS a, Tree (AS a))
  , _last'  :: AS a
  }

data LM a = LM
  { _height    :: !Int
  , _lastWidth :: !Int
  , _maxWidth  :: !Int
  , _block     :: Maybe (AS a , Tree (AS a))
  , _last      :: AS a
  } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

{-# INLINE lmToList #-}
lmToList :: LM a -> [AS a]
lmToList x = case _block x of
  Nothing       -> [_last x]
  Just (hd, tl) -> hd : toList (Node tl (_last x) Leaf)

{-# INLINE lToList #-}
lToList :: L' a -> [AS a]
lToList (L' b l) = case b of
  Nothing       -> [l]
  Just (hd, tl) -> hd : toList (Node tl l Leaf)


{-# INLINE mkBlock #-}
mkBlock :: Maybe (a, Tree a) -> a -> Tree a -> Maybe (a, Tree a)
mkBlock Nothing        y ys = Just (y, ys)
mkBlock (Just (x, xs)) y ys = Just (x, Node xs y ys)

instance Monoid a => Semigroup (LM a) where
  a <> b =
    LM { _height    = _height a + _height b
       , _lastWidth = _lastWidth a + _lastWidth b
       , _maxWidth  = max (_maxWidth a) (_maxWidth b + _lastWidth a)
       , _block     = block
       , _last      = last
       } where

    indent = mkAS (P.replicate (_lastWidth a) ' ')
    (block, last) = case _block b of
      Nothing        -> ( _block a
                        , _last a <> _last b)
      Just (hd , tl) -> ( mkBlock (_block a) (_last a <> hd)
                                  (fmap (indent <>) tl)
                        , indent <> _last b)

instance Monoid a => Semigroup (L' a) where
  a <> b = L' block last where

    indent = mkAS (P.replicate (length $ _last' a) ' ')
    (block, last) = case _block' b of
      Nothing        -> ( _block' a
                        , _last' a <> _last' b)
      Just (hd , tl) -> ( mkBlock (_block' a) (_last' a <> hd)
                                  (fmap (indent <>) tl)
                        , indent <> _last' b)

instance Semigroup (M a) where
  a <> b =
    M {maxWidth = max (maxWidth a) (maxWidth b + lastWidth a),
       height = height a + height b,
       lastWidth = lastWidth a + lastWidth b}

instance Monoid a => Monoid (LM a) where
  mempty = text ""
  mappend = (<>)

instance Monoid a => Monoid (L' a) where
  mempty = text ""
  mappend = (<>)

instance Monoid a => Monoid (M a) where
  mempty = text ""
  mappend = (<>)

instance Layout LM where
  text s = LM { _height    = 0
              , _maxWidth  = n
              , _lastWidth = n
              , _block     = Nothing
              , _last      = mkAS s
              } where n = length s
  flush x = LM { _height    = _height x + 1
               , _maxWidth  = _maxWidth x
               , _lastWidth = 0
               , _block     = mkBlock (_block x) (_last x) Leaf
               , _last      = mkAS ""
               }
  annotate a x = let ann = annotateAS a in
    x { _block = fmap (\(a,b) -> (ann a, fmap ann b)) (_block x)
      , _last  = _last x
      }

instance Layout L' where
  text s = L' Nothing (mkAS s)
  flush (L' b l) = L' (mkBlock b l Leaf) (mkAS "")
  annotate a (L' b l) = let ann = annotateAS a in
    L' { _block' = fmap (\(a,b) -> (ann a, fmap ann b)) b
       , _last'  = l
       }

instance Layout M where
  text s = M {height = 0, maxWidth = length s, lastWidth = length s}
  flush a = M {maxWidth = maxWidth a,
               height = height a + 1,
               lastWidth = 0}
  annotate _ M{..} = M{..}
class Poset a where
  (≺) :: a -> a -> Bool

instance Poset (LM a) where
  LM c1 l1 s1 _ _ ≺ LM c2 l2 s2 _ _ = c1 <= c2 && l1 <= l2 && s1 <= s2

instance Poset (M a) where
  M c1 l1 s1 ≺ M c2 l2 s2 = c1 <= c2 && l1 <= l2 && s1 <= s2

mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn m = go
  where
    go [] xs = xs
    go xs [] = xs
    go (x:xs) (y:ys)
      | m x <= m y  = x:go xs (y:ys)
      | otherwise    = y:go (x:xs) ys

mergeAllOn :: Ord b => (a -> b) -> [[a]] -> [a]
mergeAllOn _ [] = []
mergeAllOn m (x:xs) = mergeOn m x (mergeAllOn m xs)

bestsOn :: forall a b. (Measurable a b, Poset b, Ord b) => [[a]] -> [a]
bestsOn = paretoOn' m [] . mergeAllOn m where m = measure

-- | @paretoOn m = paretoOn' m []@
paretoOn' :: Poset b => (a -> b) -> [a] -> [a] -> [a]
paretoOn' _ acc [] = P.reverse acc
paretoOn' m acc (x:xs) = if any ((≺ m x) . m) acc
                            then paretoOn' m acc xs
                            else paretoOn' m (x:acc) xs
                            -- because of the ordering, we have that
                            -- for all y ∈ acc, y <= x, and thus x ≺ y
                            -- is false. No need to refilter acc.

-- list sorted by lexicographic order for the first component
-- function argument is the page width
data ODoc d a = MkDoc { fromDoc :: Int -> [d a] }

class Measurable t b | t -> b where
  measure :: t -> b

class WithSizes b where
  theWidth  :: b -> Int
  theHeight :: b -> Int

instance (Measurable (d a) b, WithSizes b, Semigroup (d a), Ord b, Poset b) =>
         Semigroup (ODoc d a) where
  MkDoc xs <> MkDoc ys = MkDoc $ \ w ->
    bestsOn [ discardInvalid w [ x <> y | y <- ys w ]
            | x <- xs w ]

discardInvalid :: (Measurable (d a) b, WithSizes b) => Int -> [d a] -> [d a]
discardInvalid w = quasifilter (fits w . measure)

quasifilter :: (Measurable (d a) b, WithSizes b) =>
               (d a -> Bool) -> [d a] -> [d a]
quasifilter _ [] = []
quasifilter p zs = let fzs = filter p zs
                     in if null fzs -- in case that there are no valid layouts, we take a narrow one.
                        then [minimumBy (compare `on` (theWidth . measure)) zs]
                        else fzs

instance ( Measurable (d a) b, WithSizes b
         , Semigroup (d a), Monoid a
         , Ord b, Poset b
         , Layout d
         ) => Monoid (ODoc d a) where
  mempty = text ""
  mappend = (<>)

fits :: WithSizes b => Int -> b -> Bool
fits w x = theWidth x <= w

instance Layout d => Layout (ODoc d) where
  text s = MkDoc $ \_ -> pure (text s)
  flush (MkDoc xs) = MkDoc $ \w -> fmap flush (xs w)
  annotate a (MkDoc xs) = MkDoc $ \w -> fmap (annotate a) (xs w)

class Layout d => Renderable d a where
  renderWith :: (Monoid r, Annotation a) => Options a r -> d a -> r

instance (Measurable (d a) b, WithSizes b, Renderable d a) =>
         Renderable (ODoc d) a where
  renderWith opts d = case xs of
    []    -> error "No suitable layout found."
    (x:_) -> renderWith opts x

    where
      pageWidth = optsPageWidth opts
      xs = discardInvalid pageWidth (fromDoc d pageWidth)

instance Renderable L a where
  renderWith = renderWithL

instance Renderable L' a where
  renderWith = renderWithL'

instance Renderable l a => Renderable (Pair M l) a where
  renderWith opts = renderWith opts . scnd

instance Renderable LM a where
  renderWith = renderWithLM

onlySingleLine :: (Measurable a b, WithSizes b) => [a] -> [a]
onlySingleLine = takeWhile (\ x -> theHeight (measure x) == 0)

spaces :: (Monoid a,Layout l) => Int -> l a
spaces n = text $ replicate n ' '


-- | The document @(x \$$> y)@ concatenates document @x@ and @y@ with
-- a linebreak in between. (infixr 5)
($$) :: (Layout d, Monoid a, Semigroup (d a)) => d a -> d a -> d a
a $$ b = flush a <> b

second f (a,b) = (a, f b)

instance WithSizes (M a) where
  theWidth  = maxWidth
  theHeight = height

instance Measurable (Pair M l a) (M a) where
  measure = frst

instance WithSizes (LM a) where
  theWidth  = _maxWidth
  theHeight = _height

instance Measurable (LM a) (LM a) where
  measure = id

{-# SPECIALISE groupingBy :: (Monoid a, Ord a) => String -> [(Int, Doc a)] -> Doc a #-}
{-# SPECIALISE groupingBy :: (Monoid a, Ord a) => String -> [(Int, Doc' a)] -> Doc' a #-}
groupingBy :: Document d a b => String -> [(Int,ODoc d a)] -> ODoc d a
groupingBy _ [] = mempty
groupingBy separator ms = MkDoc $ \w ->
  let mws = map (second (($ w) . fromDoc)) ms
      (_,lastMw) = last mws
      hcatElems = map (onlySingleLine . snd) (init mws) ++ [lastMw] -- all the elements except the first must fit on a single line
      vcatElems = map (\(indent,x) -> map (spaces indent <>) x) mws
      horizontal = discardInvalid w $ foldr1 (liftA2 (\x y -> x <> text separator <> y)) hcatElems
      vertical = foldr1 (\xs ys -> bestsOn [[x $$ y | y <- ys] | x <- xs]) vcatElems
  in bestsOn [horizontal,vertical]

data Pair f g a = (:-:) {frst :: f a, scnd :: g a}

instance (Semigroup (f a), Semigroup (g a)) => Semigroup (Pair f g a) where
  (x :-: y) <> (x' :-: y') = (x <> x') :-: (y <> y')
instance (Monoid (f a), Monoid (g a)) => Monoid (Pair f g a) where
  mempty = mempty :-: mempty
  mappend (x :-: y)(x' :-: y') = (x `mappend` x') :-: (y `mappend` y')

instance (Layout a, Layout b) => Layout (Pair a b) where
  text s = text s :-: text s
  flush (a:-:b) = (flush a:-: flush b)
  annotate x (a:-:b) = (annotate x a:-:annotate x b)

instance Monoid a => IsString (Doc a) where
  fromString = text

type Annotation a = (Monoid a)
type LDoc l a = ODoc (Pair M l) a
type Doc a = LDoc L a
type Doc' a = LDoc L' a

-- tt :: Doc ()
-- tt = groupingBy " " $ map (4,) $ 
--      ((replicate 4 $ groupingBy " " (map (4,) (map text ["fw"]))) ++
--       [groupingBy " " (map (0,) (map text ["fw","arstnwfyut","arstin","arstaruf"]))])

-- $setup
-- >>> import Text.PrettyPrint.Compact
-- >>> import Data.Monoid
-- >>> import Data.Char


class ( Layout d, Measurable (d a) b, WithSizes b
      , Semigroup (d a), Monoid a
      , Ord b, Poset b
      ) => Document d a b
      | d a -> b

instance (Monoid a, Ord a, Semigroup (l a), Layout l) => Document (Pair M l) a (M a)
instance (Monoid a, Ord a) => Document LM a (LM a)
