{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Spacing(
  Spacing
, HasSpacing(..)
, nameHeader
, meaningHeader
, sourceHeader
, scoreHeader
, standardSpacing
, mkSpacing
, (<->)
, exactWidthSpacing
) where

import Control.Category((.), id)
import Control.Lens(Lens', set, (^.))
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym(HasAcronym(name, meaning, source))
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Score(HasShowScore(showScore))
import Data.Eq(Eq)
import Data.Foldable(length, foldMap)
import Data.Function(($))
import Data.Functor(fmap)
import Data.Int(Int)
import Data.Ord(Ord, max, min)
import Data.Semigroup
import Data.String(String)
import Prelude(Show)

data Spacing =
  Spacing
    Int -- separator
    Int -- name
    Int -- meaning
    Int -- source
    Int -- score
  deriving (Eq, Ord, Show)

mkSpacing ::
  Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Spacing
mkSpacing sep n m s r =
  let b = max 1
  in  Spacing (b sep) (b n) (b m) (b s) (b r)

instance Semigroup Spacing where
  Spacing a1 b1 c1 d1 e1 <> Spacing a2 b2 c2 d2 e2 =
    Spacing (a1 `max` a2) (b1 `max` b2) (c1 `max` c2) (d1 `max` d2) (e1 `max` e2)
    
instance Monoid Spacing where
  mappend =
    (<>)
  mempty =
    mkSpacing 1 1 1 1 1

class HasSpacing a where
  spacing ::
    Lens'
      a
      Spacing
  separatorSpacing ::
    Lens'
      a
      Int
  {-# INLINE separatorSpacing #-}
  separatorSpacing =
    spacing . separatorSpacing
  nameSpacing ::
    Lens'
      a
      Int
  {-# INLINE nameSpacing #-}
  nameSpacing =
    spacing . nameSpacing
  meaningSpacing ::
    Lens'
      a
      Int
  {-# INLINE meaningSpacing #-}
  meaningSpacing =
    spacing . meaningSpacing
  sourceSpacing ::
    Lens'
      a
      Int
  {-# INLINE sourceSpacing #-}
  sourceSpacing =
    spacing . sourceSpacing
  scoreSpacing ::
    Lens'
      a
      Int
  {-# INLINE scoreSpacing #-}
  scoreSpacing =
    spacing . scoreSpacing
  
instance HasSpacing Spacing where
  spacing =
    id
  separatorSpacing
    f (Spacing a b c d e) =
      fmap (\x -> mkSpacing x b c d e) (f a)
  nameSpacing
    f (Spacing a b c d e) =
      fmap (\x -> mkSpacing a x c d e) (f b)
  meaningSpacing
    f (Spacing a b c d e) =
      fmap (\x -> mkSpacing a b x d e) (f c)
  sourceSpacing
    f (Spacing a b c d e) =
      fmap (\x -> mkSpacing a b c x e) (f d)
  scoreSpacing
    f (Spacing a b c d e) =
      fmap (\x -> mkSpacing a b c d x) (f e)

nameHeader ::
  String
nameHeader =
  "NAME"

meaningHeader ::
  String
meaningHeader =
  "MEANING"

sourceHeader ::
  String
sourceHeader =
  "SOURCE"

scoreHeader ::
  String
scoreHeader =
  "SCORE"

standardSpacing ::
  Spacing
standardSpacing =
  Spacing
    1
    (length nameHeader)
    (length meaningHeader)
    (length sourceHeader)
    (length scoreHeader)

exactWidthSpacing ::
  (HasShowScore a, HasAcronym a) =>
  [a]
  -> Spacing
exactWidthSpacing x =
  standardSpacing <>
  foldMap
    (
      \a -> 
        set nameSpacing (length (a ^. name)) .
        set meaningSpacing (length (a ^. meaning)) .
        set sourceSpacing (length (a ^. source)) .
        set scoreSpacing (length (a ^. showScore))
        $ mempty
    ) x

(<->) ::
  Spacing
  -> Spacing
  -> Spacing
Spacing a1 b1 c1 d1 e1 <-> Spacing a2 b2 c2 d2 e2 =
  Spacing (a1 `min` a2) (b1 `min` b2) (c1 `min` c2) (d1 `min` d2) (e1 `min` e2)
    