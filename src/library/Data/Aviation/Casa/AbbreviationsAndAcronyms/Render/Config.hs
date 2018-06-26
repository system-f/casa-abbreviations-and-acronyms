{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Config(
  Config(..)
, HasConfig(..)
, standardConfig
, ConfigReader(..)
, runConfig
, readColours
, readHeadingSeparatorColours
, readHeadingNameColours
, readHeadingMeaningColours
, readHeadingSourceColours
, readHeadingScoreColours
, readAcronymSeparatorColours
, readAcronymNameColours
, readAcronymMeaningColours
, readAcronymSourceColours
, readAcronymScoreColours
, readSpacing
, readSeparatorSpacing
, readNameSpacing
, readMeaningSpacing
, readSourceSpacing
, readScoreSpacing
, exactWidthSpacingStandardColours
) where

import Control.Applicative(Applicative(pure, (<*>)))
import Control.Category((.), id)
import Control.Lens(Rewrapped, Wrapped(_Wrapped'), Unwrapped, Lens', iso, (^.))
import Control.Monad(Monad(return, (>>=)))
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym(HasAcronym)
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Colours(HasColours(colours), Colours, headingSeparatorColours, headingNameColours, headingMeaningColours, headingSourceColours, headingScoreColours, acronymSeparatorColours, acronymNameColours, acronymMeaningColours, acronymSourceColours, acronymScoreColours, standardColours)
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Score(HasShowScore)
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Spacing(HasSpacing(spacing), Spacing, separatorSpacing, nameSpacing, meaningSpacing, sourceSpacing, scoreSpacing, standardSpacing, exactWidthSpacing)
import Data.Functor(Functor(fmap), (<$>))
import Data.Int(Int)
import Data.String(String)

data Config =
  Config
    Colours
    Spacing

standardConfig ::
  Config
standardConfig =
  Config
    standardColours
    standardSpacing

class HasConfig a where
  config ::
    Lens'
      a
      Config

instance HasConfig Config where
  config =
    id

instance HasColours Config where
  colours f (Config c s) =
    fmap (\c' -> Config c' s) (f c)
    
instance HasSpacing Config where
  spacing f (Config c s) =
    fmap (\s' -> Config c s') (f s)

newtype ConfigReader a =
  ConfigReader
    (Config -> a)

instance ConfigReader a_aaRr ~ t_aaRq =>
  Rewrapped (ConfigReader a_a86d) t_aaRq

instance Wrapped (ConfigReader a_a86d) where
  type Unwrapped (ConfigReader a_a86d) = Config -> a_a86d
  _Wrapped' = (iso (\(ConfigReader x_aaRp) -> x_aaRp)) ConfigReader

runConfig ::
  ConfigReader a
  -> Config
  -> a
runConfig (ConfigReader a) =
  a

instance Functor ConfigReader where
  fmap f (ConfigReader g) =
    ConfigReader (f . g)

instance Applicative ConfigReader where
  pure =
    ConfigReader . pure
  ConfigReader f <*> ConfigReader a =
    ConfigReader (\x -> f x (a x))

instance Monad ConfigReader where
  return =
    pure
  ConfigReader a >>= f =
    ConfigReader (\x -> runConfig (f (a x)) x)

readColours ::
  ConfigReader Colours
readColours =
  ConfigReader
    (^. colours)

readHeadingSeparatorColours ::
  ConfigReader (String -> String)
readHeadingSeparatorColours =
  (^. headingSeparatorColours) <$> readColours

readHeadingNameColours ::
  ConfigReader (String -> String)
readHeadingNameColours =
  (^. headingNameColours) <$> readColours

readHeadingMeaningColours ::
  ConfigReader (String -> String)
readHeadingMeaningColours =
  (^. headingMeaningColours) <$> readColours

readHeadingSourceColours ::
  ConfigReader (String -> String)
readHeadingSourceColours =
  (^. headingSourceColours) <$> readColours

readHeadingScoreColours ::
  ConfigReader (String -> String)
readHeadingScoreColours =
  (^. headingScoreColours) <$> readColours

readAcronymSeparatorColours ::
  ConfigReader (String -> String)
readAcronymSeparatorColours =
  (^. acronymSeparatorColours) <$> readColours

readAcronymNameColours ::
  ConfigReader (String -> String)
readAcronymNameColours =
  (^. acronymNameColours) <$> readColours

readAcronymMeaningColours ::
  ConfigReader (String -> String)
readAcronymMeaningColours =
  (^. acronymMeaningColours) <$> readColours

readAcronymSourceColours ::
  ConfigReader (String -> String)
readAcronymSourceColours =
  (^. acronymSourceColours) <$> readColours

readAcronymScoreColours ::
  ConfigReader (String -> String)
readAcronymScoreColours =
  (^. acronymScoreColours) <$> readColours

readSpacing ::
  ConfigReader Spacing
readSpacing =
  ConfigReader
    (^. spacing) 

readSeparatorSpacing ::
  ConfigReader Int
readSeparatorSpacing =
  (^. separatorSpacing) <$> readSpacing

readNameSpacing ::
  ConfigReader Int
readNameSpacing =
  (^. nameSpacing) <$> readSpacing

readMeaningSpacing ::
  ConfigReader Int
readMeaningSpacing =
  (^. meaningSpacing) <$> readSpacing

readSourceSpacing ::
  ConfigReader Int
readSourceSpacing =
  (^. sourceSpacing) <$> readSpacing

readScoreSpacing ::
  ConfigReader Int
readScoreSpacing =
  (^. scoreSpacing) <$> readSpacing

exactWidthSpacingStandardColours ::
  (HasShowScore a, HasAcronym a) =>
  [a]
  -> Config  
exactWidthSpacingStandardColours x =
  Config
    standardColours
    (exactWidthSpacing x)
