module AbbreviationsAndAcronymsGenerate where

import qualified Data.ByteString.Lazy.Char8 as LazyChar8ByteString
import Control.Lens
import Control.Monad
import Data.Char
import Network.Wreq
import System.Environment

getAbbreviationPage ::
  String
  -> IO String
getAbbreviationPage x =
  do  q <- get ("http://www.casa.gov.au/about-us/standard-page/aviation-abbreviations-and-acronyms?page=" ++ x)
      let b = LazyChar8ByteString.unpack (q ^. responseBody)
      pure b

trim' =
         escapeChars . reverse . dropWhile isSpace . drop 15 . reverse . dropWhile isSpace . drop 12

getAcronyms ::
  [String]
  -> [Acronym]
getAcronyms [] =
  []
getAcronyms (i1:a:i2:b:i3:c:i4:r) =
  let matches =
        [
          (i1, "                  <td class=\"views-field views-field-title active\" >")
        , (i2, "                  <td class=\"views-field views-field-field-meaning\" >")
        , (i3, "                  <td class=\"views-field views-field-field-source\" >")
        , (i4, "              </tr>")
        ]
      trim =
         escapeChars . reverse . dropWhile isSpace . drop 15 . reverse . dropWhile isSpace . drop 12
  in  if all (uncurry (==)) matches
        then
          Acronym
            (trim a)
            (trim b)
            (trim c)
          :getAcronyms r
        else
          getAcronyms (a:i2:b:i3:c:i4:r)
getAcronyms (_:t) =
  getAcronyms t

requestAcronyms ::
  [Int]
  -> IO [Acronym]
requestAcronyms x =
  join <$> traverse (((getAcronyms . lines) <$>) . getAbbreviationPage . show) x

requestAllAcronyms ::
  IO [Acronym]
requestAllAcronyms =
  requestAcronyms [0..49]

data Acronym =
  Acronym
    String -- acronym
    String -- meaning
    String -- source
  deriving (Eq, Ord, Show)

render ::
  [Acronym]
  -> String
render acrs =
  let render1 (Acronym acr mean src) =
        concat
          [ 
            "Acronym\n"
          , "      \""
          , acr
          , "\"\n"
          , "      \""
          , mean
          , "\"\n"
          , "      \""
          , src
          , "\""
          ]
      allAcronyms =
        concat
          [
            "allAcronyms =\n"
          , "  [\n"
          , zip (True:cycle [False]) acrs >>= \(p, a) ->
              concat
                [
                  "  "
                , if p then " " else ","
                , " "
                , render1 a
                , "\n"
                ]
          , "  ]\n"
          ]
  in  allAcronyms

escapeChars ::
  String
  -> String
escapeChars =
  transform
    (\x ->  case x of
              '&':'#':'0':'3':'9':';':r ->
                '\'':r
              h:t ->
                if isSpace h
                  then
                    h : dropWhile isSpace t
                  else
                    x
              _ ->
                x
              )

main ::
  IO ()
main =
  do  a <- getArgs
      let n =
            case a of
              [] ->
                [0..49]
              q@(_:_) ->
                read <$> q
      c <- requestAcronyms n
      writeFile "acronyms.hs" (render c)
