{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Score(
  HasScore(..)
, HasShowScore(..)
) where

import Control.Category(id)
import Control.Lens(Lens', Getter, to)
import Data.Functor(fmap)
import Data.Int(Int)
import Data.Monoid.Textual(TextualMonoid)
import Data.String(String)
import Prelude(show)
import Text.Fuzzy(Fuzzy(Fuzzy))

class HasScore a where
  score ::
    Lens'
      a
      Int

instance HasScore Int where
  score =
    id

instance TextualMonoid s => HasScore (Fuzzy a s) where
  score f (Fuzzy x o s) =
    fmap (\t -> Fuzzy x o t) (f s)

class HasShowScore a where
  showScore ::
    Getter
      a
      String

instance HasShowScore String where
  showScore =
    id

instance HasShowScore Int where
  showScore =
    to show

instance TextualMonoid s => HasShowScore (Fuzzy a s) where
  showScore =
    to (\(Fuzzy _ _ s) -> show s)
