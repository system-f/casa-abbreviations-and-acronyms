{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Applicative((<*>), (<**>))
import Control.Category((.), id)
import Control.Lens((%~), lens)
import Data.Bool(bool)
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym(HasAcronym(acronym), Acronym)
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render(renderHeaderAcronyms, renderAcronyms)
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Colours(Colours, standardColours)
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Config(ConfigReader, Config(Config), runConfig)
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Score(HasShowScore(showScore))
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Spacing(nameSpacing, meaningSpacing, sourceSpacing, scoreSpacing, exactWidthSpacing)
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Search(searchFuzzyNameMeaningSource, searchIndexNameMeaningSource, searchFuzzyNameMeaning, searchIndexNameMeaning, searchFuzzyNameSource, searchIndexNameSource, searchFuzzyName, searchIndexName)
import Data.Bool(Bool(False, True))
import Data.Eq(Eq)
import Data.Foldable(foldr)
import Data.Function(($))
import Data.Functor((<$>), fmap)
import Data.Int(Int)
import Data.List(filter)
import Data.Maybe(Maybe(Just, Nothing), maybe, maybeToList)
import Data.Monoid(Monoid(mempty))
import Data.Ord(Ord((>=), (>)), max, min)
import Data.Semigroup((<>))
import Data.String(String)
import Data.Traversable(Traversable)
import Options.Applicative(Parser, execParser, info, helper, fullDesc, header, option, maybeReader, short, long, value, help, switch, strOption)
import Prelude(Show(show))
import System.IO(IO, putStrLn)
import Text.Fuzzy(Fuzzy(Fuzzy))
import Text.Read(reads)

main ::
  IO ()
main =
  let execopts =
        execParser
          (info ((parserOptions :: Parser (Options [] ShowAcronym)) <**> helper) (
            fullDesc <>
            header "casa-abbreviations-and-acronyms for searching CASA abbreviations and acronyms <https://www.casa.gov.au/about-us/standard-page/aviation-abbreviations-and-acronyms>"
          )
        )
  in  do  opts <- execopts
          case opts of
            Options clrs rndr (MatchField fz ex) typ (FieldSpacing mn xn mm xm ms xs mr xr) term ->
              let acro =
                    let match =
                          case typ of
                            ExactMatch ->
                              fmap (\a -> ShowAcronym a "-") . maybeToList . ex
                            InexactMatch x ->
                              fmap (\(Fuzzy o _ s) -> ShowAcronym o (show s)) . maybe id (\n -> filter (\(Fuzzy _ _ s) -> s >= n)) x . fz
                    in  match term
                  space =
                    foldr
                      (\(a, b, c) w ->  let k val func = a %~ maybe id func val
                                        in  if b > c
                                              then
                                                id
                                              else
                                                k b max . k c min . w)
                      id
                      [
                        (nameSpacing, mn, xn)
                      , (meaningSpacing, mm, xm)
                      , (sourceSpacing, ms, xs)
                      , (scoreSpacing, mr, xr)
                      ]
                  out =
                    runConfig
                      (rndr acro)
                      (space $ Config clrs (exactWidthSpacing acro))
              in  putStrLn out

data ShowAcronym =
  ShowAcronym
    Acronym
    String -- score
  deriving (Eq, Ord, Show)

instance HasAcronym ShowAcronym where
  acronym =
    lens
      (\(ShowAcronym a _) -> a)
      (\(ShowAcronym _ s) a -> ShowAcronym a s)
      
instance HasShowScore ShowAcronym where
  showScore =
    lens
      (\(ShowAcronym _ s) -> s)
      (\(ShowAcronym a _) s -> ShowAcronym a s)

data MatchField =
  MatchField
    (String -> [Fuzzy Acronym String])
    (String -> Maybe Acronym)

matchField' ::
  (String -> String -> String -> Bool -> [Fuzzy Acronym String])
  -> (String -> Maybe Acronym)
  -> MatchField
matchField' f g =
  MatchField (\s -> f s "" "" False) g

parserMatchField ::
  Parser MatchField
parserMatchField =
  (
    \p q -> 
        case p of
      False ->
        case q of
          False ->
            matchField'
              searchFuzzyNameMeaningSource
              searchIndexNameMeaningSource
          True ->
            matchField'
              searchFuzzyNameMeaning
              searchIndexNameMeaning
      True ->
        case q of
          False ->
            matchField'
              searchFuzzyNameSource
              searchIndexNameSource
          True ->
            matchField'
              searchFuzzyName
              searchIndexName
  ) <$>
    switch
      (
        long "no-match-meaning" <>
        long "nm" <>
        help "do not match the acronym meaning"
      )
    <*>
        switch
      (
        long "no-match-source" <>
        long "ns" <>
        help "do not match the acronym source"
      )

data MatchType =
  ExactMatch
  | InexactMatch (Maybe Int)
  deriving (Eq, Ord, Show)

parserMatchType ::
  Parser MatchType
parserMatchType =
  let opts exact minscore =
        if exact
          then
            ExactMatch
          else
            InexactMatch minscore
  in  opts <$>
        switch
          (
            short 'e' <>
            long "exact" <>
            help "match the search term exactly"
          ) <*>
        option
          (
            maybeReader
              (\s -> case reads s of
                        (n, _):_ ->
                          Just (Just n)
                        [] ->
                          Nothing)
          )
          (
            short 's' <>
            long "min-score" <>
            value Nothing <>
            help "minimum fuzzy match score"
          )

data FieldSpacing =
  FieldSpacing
    (Maybe Int) -- min name
    (Maybe Int) -- max name
    (Maybe Int) -- min meaning
    (Maybe Int) -- max meaning
    (Maybe Int) -- min source
    (Maybe Int) -- max source
    (Maybe Int) -- min score
    (Maybe Int) -- max score
  deriving (Eq, Ord, Show)    

parserFieldSpacing ::
  Parser FieldSpacing
parserFieldSpacing =
  let minmaxWidth longname1 longname2 helptext =
        option
          (
            maybeReader
              (\s -> case reads s of
                        (n, _):_ ->
                          Just (Just n)
                        [] ->
                          Nothing)

          )
          (
            long longname1 <>
            long longname2 <>
            value Nothing <>
            help helptext
          )
  in  FieldSpacing <$>
        minmaxWidth "min-name-width" "mn" "minimum acronym name column width"
        <*>
        minmaxWidth "max-name-width" "xn" "maximum acronym name column width"
        <*>
        minmaxWidth "min-meaning-width" "mm" "minimum acronym meaning column width"
        <*>
        minmaxWidth "max-meaning-width" "xm" "maximum acronym meaning column width"
        <*>
        minmaxWidth "min-source-width" "ms" "minimum acronym source column width"
        <*>
        minmaxWidth "max-source-width" "xs" "maximum acronym source column width"
        <*>
        minmaxWidth "min-score-width" "mr" "minimum score column width"
        <*>
        minmaxWidth "max-score-width" "xr" "maximum score column width"
        
data Options t a =
  Options
    Colours -- no colours
    (t a -> ConfigReader String) -- no header
    MatchField
    MatchType
    FieldSpacing
    String -- the search term
  
parserOptions ::
  (HasShowScore a, HasAcronym a, Traversable t) =>
  Parser (Options t a)
parserOptions =
  Options <$>
    (
      bool standardColours mempty <$>
      switch
        (
          short 'c' <>
          long "no-colour" <>
          help "turn off ANSI escape code colours"
        )
    )
    <*>
    (
      (bool renderHeaderAcronyms renderAcronyms) <$>
      switch
        (
          short 'h' <>
          long "no-header" <>
          help "turn off the header in the output"
        )
    )
    <*>
    parserMatchField
    <*>
    parserMatchType
    <*>
    parserFieldSpacing
    <*>
    strOption
      (
        short 't' <>
        long "term" <>
        help "the search term"
      )
