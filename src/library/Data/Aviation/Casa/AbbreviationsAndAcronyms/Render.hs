{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Render(
  renderHeader
, renderAcronym
, renderAcronyms
, renderHeaderAcronyms
) where


import Control.Applicative(pure)
import Control.Category((.))
import Control.Lens((^.), transform)
import Control.Monad((>>=))
import Data.Align(Align(alignWith))
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Config(ConfigReader, readHeadingSeparatorColours, readSeparatorSpacing, readHeadingNameColours, readNameSpacing, readHeadingMeaningColours, readMeaningSpacing, readHeadingSourceColours, readSourceSpacing, readHeadingScoreColours, readScoreSpacing, readAcronymSeparatorColours, readAcronymNameColours, readAcronymMeaningColours, readAcronymSourceColours, readAcronymScoreColours)
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Score(HasShowScore(showScore))
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Spacing(nameHeader, meaningHeader, sourceHeader, scoreHeader)
import Data.Int(Int)
import Data.Foldable(toList, length)
import Data.Function(($))
import Data.Functor((<$>))
import Data.List(intercalate, replicate, (++), concat, take, splitAt)
import Data.List.NonEmpty(NonEmpty, (<|))
import Data.String(String)
import Data.These(These(This, That, These))
import Data.Traversable(Traversable(traverse))
import Prelude((-))

renderHeader ::
  ConfigReader String
renderHeader =
  do  chc <- readHeadingSeparatorColours
      shc <- readSeparatorSpacing
      chn <- readHeadingNameColours
      shn <- readNameSpacing
      chm <- readHeadingMeaningColours
      shm <- readMeaningSpacing
      chs <- readHeadingSourceColours
      shs <- readSourceSpacing
      chr <- readHeadingScoreColours
      shr <- readScoreSpacing
      pure . intercalate (chc (replicate shc '|')) $
        [
          chn (spaceN shn nameHeader)
        , chm (spaceN shm meaningHeader)
        , chs (spaceN shs sourceHeader)
        , chr (spaceN shr scoreHeader)
        ]

renderAcronym ::
  (HasShowScore a, HasAcronym a) =>
  a
  -> ConfigReader String
renderAcronym a =
  let name' =
        escapeChars (a ^. name)
      meaning' =
        escapeChars (a ^. meaning)
      source' =
        escapeChars (a ^. source)
      score' =
        a ^. showScore
      spacesplit n x =
        toList $ spaceN n <$> splitEvery n x
  in  do  chc <- readAcronymSeparatorColours
          shc <- readSeparatorSpacing
          chn <- readAcronymNameColours
          shn <- readNameSpacing
          chm <- readAcronymMeaningColours
          shm <- readMeaningSpacing
          chs <- readAcronymSourceColours
          shs <- readSourceSpacing
          chr <- readAcronymScoreColours
          shr <- readScoreSpacing
          let name'' =
                spacesplit shn name'
              meaning'' =
                spacesplit shm meaning'
              source'' =
                spacesplit shs source'
              score'' =
                spacesplit shr score'
              alignWidth ::
                Align f =>
                (String -> String -> a)
                -> f String
                -> f String
                -> String
                -> String
                -> f a
              alignWidth k m n ms ns =
                alignWith
                  (\t -> case t of
                            This a1 ->
                              a1 `k` ns
                            That a2 ->
                              ms `k` a2
                            These a1 a2 ->
                              a1 `k` a2)
                  m
                  n
              spacers a1 a2 =
                a1 ++ chc (replicate shc '|') ++ a2
              column4 =
                let hn' =
                      chn (replicate shn ' ')
                    hm' =
                      chm (replicate shm ' ')
                    hs' =
                      chs (replicate shs ' ')
                    hr' =
                      chr (replicate shr ' ')
                    column12 =
                      alignWidth spacers (chn <$> name'') (chm <$> meaning'') hn' hm'
                    column3 =
                      alignWidth spacers column12 (chs <$> source'') (hn' ++ hm') hs'
                in  alignWidth spacers column3 (chr <$> score'') (hn' ++ hm' ++ hs') hr'
          pure (newlines column4)

renderAcronyms ::
  (Traversable t, HasAcronym a, HasShowScore a) =>
  t a
  -> ConfigReader String
renderAcronyms as =
  concat <$> traverse renderAcronym as

renderHeaderAcronyms ::
  (Traversable t, HasAcronym a, HasShowScore a) =>
  t a
  -> ConfigReader String
renderHeaderAcronyms as=
  do  h <- renderHeader
      a <- renderAcronyms as
      pure (h ++ "\n" ++ a)

spaceN ::
  Int
  -> String
  -> String
spaceN n x =
  let n' = n - length x
  in  take n x ++ replicate n' ' '

escapeChars ::
  String
  -> String
escapeChars =
  transform
    (\x ->  case x of
              '&':'l':'t':';':r ->
                '<':r
              '&':'g':'t':';':r ->
                '>':r
              '&':'a':'m':'p':';':r ->
                '&':r
              '&':'q':'u':'o':'t':';':r ->
                '"':r
              _ ->
                x
              )

splitEvery ::
  Int
  -> String
  -> NonEmpty String
splitEvery w x =
  let (i, j) = splitAt w x
      k =
        case j of
          [] ->
            pure
          _:_ ->
            (<| splitEvery w j)
  in  k i

newlines ::
  [String]
  -> String
newlines s =
  s >>= (\t -> t ++ "\n")
