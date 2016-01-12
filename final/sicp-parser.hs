{-# OPTIONS_GHC -Wall #-}

{- Parser for a subset of Texinfo in sicp.texi
   ===========================================

   Copyleft 2016 Andres Raba,
   GNU General Public License, version 3. -}


module Main where

import Text.Parsec
import Text.Parsec.Text
import Control.Applicative hiding (many, (<|>))
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

-- Texinfo data types
----------------------
type Texinfo = [TexiFragment]

type Tag = String
type EndTag = String
type Symbol = String
type ExcludedChars = String
type Expression = String
type Markup = String

data TexiFragment = Void
                  | Comment String
                  | Plain String
                  | Special Symbol
                  | Single Tag
                  | NoArg Tag
                  | Braced Tag Texinfo
                  | Math Expression
                  | Line Tag Texinfo
                  | Env Tag Texinfo
                  | TeX Markup
                  deriving Show

-- The compound parser to handle a Texinfo file
------------------------------------------------
texinfo :: Parser Texinfo
texinfo = many texiFragment

texiFragment :: Parser TexiFragment
texiFragment = plainText <|> atClause <|> special

-- Plaintext fragment that doesn't contain @-clauses or special symbols
------------------------------------------------------------------------
plainText :: Parser TexiFragment
plainText = Plain <$> simpleText "@{}%$"
               
simpleText :: ExcludedChars -> Parser String
simpleText excl = concat <$>
                  many1 (nestedBraces excl <|> many1 (noneOf excl))

nestedBraces :: ExcludedChars -> Parser String
nestedBraces excl = try (string "{}") <|>
                    (\x y z -> x ++ y ++ z) <$>
                    string "{" <*> simpleText excl <*> string "}"

-- Special symbol ('%' or '$')
-------------------------------
special :: Parser TexiFragment
special = (Special . (:[])) <$> oneOf "%$"

-- A clause beginning with '@'
-------------------------------
atClause :: Parser TexiFragment
atClause = (*>) (char '@') $
           tryWith (\p -> Braced  <$>  p <*> bracedArg)     bracedTags  <|>
           tryWith (\p -> Math    <$> (p  *> mathArg "{}")) mathTags    <|>
           tryWith (\p -> Line    <$>  p <*> lineArg)       lineTags    <|>
           tryWith (\p -> Single  <$>  p)                   singleTags  <|>
           tryWith (\p -> Comment <$> (p  *> commentArg))   commentTags <|>
           tryWith (\p -> NoArg   <$>  p <*  string "{}")   emptyTags   <|>
           try ((\(t, a) -> Env t a) <$> env envTags texiFragment)      <|>
           try ((\(_, a) -> TeX a)   <$> env texTags anyChar)

tryWith :: (Parser Tag -> Parser a) -> [Tag] -> Parser a
tryWith transform = choice . map (try . transform . string)

bracedArg :: Parser Texinfo
bracedArg = char '{' *> texinfo <* char '}'

mathArg :: ExcludedChars -> Parser String
mathArg excl = char '{' *> simpleText excl <* char '}'

-- Special treatment of fragments inside one-line arguments
-- (newline is not allowed)
------------------------------------------------------------
emptyLine :: Parser String
emptyLine = manyTill space (newline <|> eof *> pure ' ')

void :: Parser Texinfo
void = emptyLine *> pure [Void]

orMany :: Parser [a] -> Parser a -> Parser [a]
vp `orMany` p = try vp <|> space *> {- spaces *> -} manyTill p newline
-- vp is void-parser or empty-line parser, p should match something else
infixl 3 `orMany`

-- Line arguments should not contain line-clauses nor multiline environments
oneLiner :: Parser TexiFragment
oneLiner = Plain <$> simpleText "@{}%$\n"                                 <|>
           special                                                        <|>
           (char '@') *>
           (tryWith (\p -> Single  <$> p)                      singleTags <|>
            tryWith (\p -> NoArg   <$> p <* string "{}")       emptyTags  <|>
            tryWith (\p -> Braced  <$> p <*>
             (char '{' *> many oneLiner <* char '}'))          bracedTags <|>
            tryWith (\p -> Math    <$> (p *> mathArg "{}\n"))  mathTags   <|>
            tryWith (\p -> Comment <$> (p *> notEOL))          commentTags)

notEOL :: Parser String
notEOL = option "" $ many1 (char ' ') *> many (noneOf "\n")

lineArg :: Parser Texinfo
lineArg = void `orMany` oneLiner

-- Comment line argument should be left as it is
-------------------------------------------------
commentArg :: Parser String
commentArg = emptyLine `orMany` noneOf "\n"

-- Environments
----------------
env :: [Tag] -> Parser a -> Parser (Tag, [a])
env tags p = choice $ map (envSelect p) tags

envSelect :: Parser a -> Tag -> Parser (Tag, [a])
envSelect p tag = (,) <$> (try . string) tag <* spaces {- <* emptyLine -} <*>
                (manyTill p $ endTag tag)

endTag :: Tag -> Parser EndTag
endTag tag = try $ string "@end " <* spaces <* string tag {- <* emptyLine -}

-- Lists of tags in each category
----------------------------------
singleTags :: [Tag]
singleTags = map (:[]) "/-`^\"{}@*'|," ++ words "short thispage"

emptyTags :: [Tag]
emptyTags = words "TeX copyright dots"

bracedTags :: [Tag]
bracedTags = words "code ref anchor strong newterm footnote i acronym var r b cite emph image url w value dfn file t titlefont"

mathTags :: [Tag]
mathTags = words "math"

lineTags :: [Tag]
lineTags = words "noindent sp item subsubheading author bullet bye center chapter cindex dircategory endpage everyheading finalout heading include node printindex section setfilename setshortcontentsaftertitlepage settitle set subsection subsubsection subtitle title unnumbered"

commentTags :: [Tag]
commentTags = words "c comment"

envTags :: [Tag]
envTags = words "detailmenu direntry enumerate example float ifinfo iftex itemize lisp macro menu quotation smallexample smalllisp titlepage"

texTags :: [Tag]
texTags = words "tex"

-- Read the Texinfo source from file
-------------------------------------
main :: IO ()
main = Tio.readFile infile >>=
       Tio.writeFile outfile . T.pack . show . parse texinfo infile

infile :: String
infile = "sicp.texi"

outfile :: String
outfile = "parsed-sicp.txt"
