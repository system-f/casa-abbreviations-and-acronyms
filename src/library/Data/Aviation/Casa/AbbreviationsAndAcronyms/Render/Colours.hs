{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Colours(
  Colours(..)
, traverseAllColours
, traverseSeparatorColours
, traverseNameColours
, traverseMeaningColours
, traverseSourceColours
, traverseScoreColours
, traverseHeadingColours
, traverseAcronymColours
, HasColours(..)
, standardColours
) where

import Control.Applicative((<*>), pure)
import Control.Category((.), id)
import Control.Lens(Traversal', Lens')
import Data.Functor(fmap, (<$>))
import Data.List((++))
import Data.Semigroup(Semigroup((<>)), Monoid(mappend, mempty))
import Data.String(String)

data Colours =
  Colours
    (String -> String) -- heading separator
    (String -> String) -- heading name
    (String -> String) -- heading meaning
    (String -> String) -- heading source
    (String -> String) -- heading score
    (String -> String) -- acronym separator
    (String -> String) -- acronym name
    (String -> String) -- acronym meaning
    (String -> String) -- acronym source
    (String -> String) -- acronym score

traverseAllColours ::
  Traversal'
    Colours
    (String -> String)
traverseAllColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> f hc <*> f hn <*> f hm <*> f hs <*> f hr <*> f ac <*> f an <*> f am <*> f as <*> f ar

traverseSeparatorColours ::
  Traversal'
    Colours
    (String -> String)
traverseSeparatorColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> f hc <*> pure hn <*> pure hm <*> pure hs <*> pure hr <*> f ac <*> pure an <*> pure am <*> pure as <*> pure ar

traverseNameColours ::
  Traversal'
    Colours
    (String -> String)
traverseNameColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> pure hc <*> f hn <*> pure hm <*> pure hs <*> pure hr <*> pure ac <*> f an <*> pure am <*> pure as <*> pure ar

traverseMeaningColours ::
  Traversal'
    Colours
    (String -> String)
traverseMeaningColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> pure hc <*> pure hn <*> f hm <*> pure hs <*> pure hr <*> pure ac <*> pure an <*> f am <*> pure as <*> pure ar

traverseSourceColours ::
  Traversal'
    Colours
    (String -> String)
traverseSourceColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> pure hc <*> pure hn <*> pure hm <*> f hs <*> pure hr <*> pure ac <*> pure an <*> pure am <*> f as <*> pure ar

traverseScoreColours ::
  Traversal'
    Colours
    (String -> String)
traverseScoreColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> pure hc <*> pure hn <*> pure hm <*> pure hs <*> f hr <*> pure ac <*> pure an <*> pure am <*> pure as <*> f ar

traverseHeadingColours ::
  Traversal'
    Colours
    (String -> String)
traverseHeadingColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> f hc <*> f hn <*> f hm <*> f hs <*> f hr <*> pure ac <*> pure an <*> pure am <*> pure as <*> pure ar

traverseAcronymColours ::
  Traversal'
    Colours
    (String -> String)
traverseAcronymColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> pure hc <*> pure hn <*> pure hm <*> pure hs <*> pure hr <*> f ac <*> f an <*> f am <*> f as <*> f ar

instance Semigroup Colours where
  Colours hc1 hn1 hm1 hs1 hr1 ac1 an1 am1 as1 ar1 <> Colours hc2 hn2 hm2 hs2 hr2 ac2 an2 am2 as2 ar2 =
    Colours (hc1 . hc2) (hn1 . hn2) (hm1 . hm2) (hs1 . hs2) (hr1 . hr2) (ac1 . ac2) (an1 . an2) (am1 . am2) (as1 . as2) (ar1 . ar2)

instance Monoid Colours where
  mappend =
    (<>)
  mempty =
    Colours
      id
      id
      id
      id
      id
      id
      id
      id
      id
      id

class HasColours a where
  colours ::
    Lens'
      a
      Colours
  headingSeparatorColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingSeparatorColours #-}
  headingSeparatorColours =
    colours . headingSeparatorColours
  headingNameColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingNameColours #-}
  headingNameColours =
    colours . headingNameColours
  headingMeaningColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingMeaningColours #-}
  headingMeaningColours =
    colours . headingMeaningColours
  headingSourceColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingSourceColours #-}
  headingSourceColours =
    colours . headingSourceColours
  headingScoreColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingScoreColours #-}
  headingScoreColours =
    colours . headingScoreColours
  acronymSeparatorColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymSeparatorColours #-}
  acronymSeparatorColours =
    colours . acronymSeparatorColours
  acronymNameColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymNameColours #-}
  acronymNameColours =
    colours . acronymNameColours
  acronymMeaningColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymMeaningColours #-}
  acronymMeaningColours =
    colours . acronymMeaningColours
  acronymSourceColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymSourceColours #-}
  acronymSourceColours =
    colours . acronymSourceColours
  acronymScoreColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymScoreColours #-}
  acronymScoreColours =
    colours . acronymScoreColours

instance HasColours Colours where
  colours =
    id
  headingSeparatorColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours x hn hm hs hr ac an am as ar) (f hc)
  headingNameColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc x hm hs hr ac an am as ar) (f hn)
  headingMeaningColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn x hs hr ac an am as ar) (f hm)
  headingSourceColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm x hr ac an am as ar) (f hs)
  headingScoreColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs x ac an am as ar) (f hr)
  acronymSeparatorColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs hr x an am as ar) (f ac)
  acronymNameColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs hr ac x am as ar) (f an)
  acronymMeaningColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs hr ac an x as ar) (f am)
  acronymSourceColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs hr ac an am x ar) (f as)
  acronymScoreColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs hr ac an am as x) (f ar)

standardColours ::
  Colours
standardColours =
  Colours
    (\s -> "\ESC[32m\ESC[42m" ++ s ++ "\ESC[m")
    (\s -> "\ESC[37m\ESC[105m" ++ s ++ "\ESC[m")
    (\s -> "\ESC[37m\ESC[105m" ++ s ++ "\ESC[m")
    (\s -> "\ESC[37m\ESC[105m" ++ s ++ "\ESC[m")
    (\s -> "\ESC[37m\ESC[105m" ++ s ++ "\ESC[m")
    (\s -> "\ESC[32m\ESC[42m" ++ s ++ "\ESC[m")
    (\s -> "\ESC[37m\ESC[100m" ++ s ++ "\ESC[m")
    (\s -> "\ESC[37m\ESC[100m" ++ s ++ "\ESC[m")
    (\s -> "\ESC[37m\ESC[100m" ++ s ++ "\ESC[m")
    (\s -> "\ESC[37m\ESC[100m" ++ s ++ "\ESC[m")
