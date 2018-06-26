{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Search(
  all_Acronym_name_index
, all_Acronym_meaning_index
, all_Acronym_source_index
, searchIndexName
, searchIndexMeaning
, searchIndexSource
, searchIndexNameSource
, searchIndexNameMeaning
, searchIndexMeaningSource
, searchIndexNameMeaningSource
, searchFuzzyName
, searchFuzzyMeaning
, searchFuzzySource
, searchFuzzyNameMeaning
, searchFuzzyNameSource
, searchFuzzyMeaningSource
, searchFuzzyNameMeaningSource
) where

import Control.Applicative((<|>))
import Control.Category((.))
import Control.Lens((^.))
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym(Acronym(Acronym), allAcronyms, name, meaning, source)
import Data.Bool(Bool)
import Data.Char(isAlpha, toUpper)
import Data.Foldable(foldl')
import Data.Function(($))
import Data.Functor((<$>), fmap)
import Data.List(sortBy, filter)
import Data.Map(Map)
import qualified Data.Map as Map(fromList, lookup, insertWith, toList)
import Data.Maybe(Maybe)
import Data.Ord(Ord((>)), compare)
import Data.String(String)
import Data.Monoid.Textual(TextualMonoid)
import qualified Text.Fuzzy as Fuzzy(filter, score)
import Text.Fuzzy(Fuzzy(Fuzzy))

all_Acronym_name_index ::
  Map String (String, String)
all_Acronym_name_index =
  Map.fromList ((\(Acronym _name _meaning _src) -> (_name, (_meaning, _src))) <$> allAcronyms)

all_Acronym_meaning_index ::
  Map String (String, String)
all_Acronym_meaning_index =
  Map.fromList ((\(Acronym _name _meaning _src) -> (_meaning, (_name, _src))) <$> allAcronyms)

all_Acronym_source_index ::
  Map String (String, String)
all_Acronym_source_index =
  Map.fromList ((\(Acronym _name _meaning _src) -> (_src, (_name, _meaning))) <$> allAcronyms)

searchIndexName ::
  String
  -> Maybe Acronym
searchIndexName s =
  let s' = filter isAlpha . fmap toUpper $ s
  in  (\(_meaning, _src) -> Acronym s _meaning _src) <$> Map.lookup s' all_Acronym_name_index

searchIndexMeaning ::
  String
  -> Maybe Acronym
searchIndexMeaning s =
  let s' = filter isAlpha . fmap toUpper $ s
  in  (\(_name, _src) -> Acronym _name s _src) <$> Map.lookup s' all_Acronym_meaning_index

searchIndexSource ::
  String
  -> Maybe Acronym
searchIndexSource s =
  let s' = filter isAlpha . fmap toUpper $ s
  in  (\(_name, _meaning) -> Acronym s _name _meaning) <$> Map.lookup s' all_Acronym_source_index

searchIndexNameMeaning ::
  String
  -> Maybe Acronym
searchIndexNameMeaning s =
  searchIndexName s <|> searchIndexMeaning s
  
searchIndexNameSource ::
  String
  -> Maybe Acronym
searchIndexNameSource s =
  searchIndexName s <|> searchIndexSource s

searchIndexMeaningSource ::
  String
  -> Maybe Acronym
searchIndexMeaningSource s =
  searchIndexMeaning s <|> searchIndexSource s

searchIndexNameMeaningSource ::
  String
  -> Maybe Acronym
searchIndexNameMeaningSource s =
  searchIndexName s <|> searchIndexMeaning s <|> searchIndexSource s
  
searchFuzzyName ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyName s before after cas =
  Fuzzy.filter s allAcronyms before after (^. name) cas

searchFuzzyMeaning ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyMeaning s before after cas =
  Fuzzy.filter s allAcronyms before after (^. meaning) cas

searchFuzzySource ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzySource s before after cas =
  Fuzzy.filter s allAcronyms before after (^. source) cas

searchFuzzyNameMeaning ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyNameMeaning s before after cas =
  filterN s allAcronyms before after [(^. name), (^. meaning)] cas
  
searchFuzzyNameSource ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyNameSource s before after cas =
  filterN s allAcronyms before after [(^. name), (^. source)] cas

searchFuzzyMeaningSource ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyMeaningSource s before after cas =
  filterN s allAcronyms before after [(^. meaning), (^. source)] cas

searchFuzzyNameMeaningSource ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyNameMeaningSource s before after cas =
  filterN s allAcronyms before after [(^. name), (^. meaning), (^. source)] cas

-- https://hackage.haskell.org/package/fuzzy-0.1.0.0/docs/Text-Fuzzy.html#v:filter
filterN ::
  (Ord t, TextualMonoid s) =>
  s
  -> [t]
  -> s
  -> s
  -> [t -> s]
  -> Bool
  -> [Fuzzy t s]
filterN _ _ _ _ [] _ =
  []
filterN pattern values before after (e:es) cas =
  let x1 = Fuzzy.filter pattern values before after e cas
      x1' = Map.fromList ((\(Fuzzy o r s) -> (o, (r, s))) <$> x1)
      x2' = foldl' (\m e' ->  let x2 = Fuzzy.filter pattern values before after e' cas
                              in foldl' (\m' (Fuzzy o r s) -> Map.insertWith (\(s1, i1) (s2, i2) ->
                                    if i2 > i1 then (s2, i2) else (s1, i1)) o (r, s) m') m x2) x1' es
  in  sortBy (\f1 f2 -> Fuzzy.score f2 `compare` Fuzzy.score f1) ((\(o, (r, s)) -> Fuzzy o r s) <$> Map.toList x2')
