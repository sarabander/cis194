{-# OPTIONS_GHC -Wall #-}

{- Parser for a subset of Texinfo in sicp.texi
   ===========================================

   Copyleft 2016 Andres Raba,
   GNU General Public License, version 3. -}


import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative

-- Texinfo data types
----------------------
type Texinfo = [TexiFragment]

type Tag = String
type EndTag = String
type Text = String
type Symbol = String
type ExcludedChars = String
type Expression = String
type Markup = String

data TexiFragment = Void
                  | Comment Text
                  | Plain Text
                  | Special Symbol
                  | Single Tag
                  | Empty Tag
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
               
simpleText :: ExcludedChars -> Parser Text
simpleText excl = concat <$>
                  many1 (nestedBraces excl <|> many1 (noneOf excl))

nestedBraces :: ExcludedChars -> Parser Text
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
           tryWith (\p -> Single  <$>  p)                   singleTags  <|>
           tryWith (\p -> Empty   <$>  p <*  string "{}")   emptyTags   <|>
           tryWith (\p -> Braced  <$>  p <*> bracedArg)     bracedTags  <|>
           tryWith (\p -> Math    <$> (p  *> mathArg "{}")) mathTags    <|>
           tryWith (\p -> Line    <$>  p <*> lineArg)       lineTags    <|>
           tryWith (\p -> Comment <$> (p  *> commentArg))   commentTags <|>
           try ((\(t, a) -> Env t a) <$> env envTags texiFragment)      <|>
           try ((\(_, a) -> TeX a)   <$> env texTags anyChar)

tryWith :: (Parser Tag -> Parser a) -> [Tag] -> Parser a
tryWith transform = choice . map (try . transform . string)

bracedArg :: Parser Texinfo
bracedArg = char '{' *> texinfo <* char '}'

mathArg :: ExcludedChars -> Parser Text
mathArg excl = char '{' *> simpleText excl <* char '}'

-- Special treatment of fragments inside one-line arguments
-- (newline is not allowed)
------------------------------------------------------------
emptyLine :: Parser Text
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
            tryWith (\p -> Empty   <$> p <* string "{}")       emptyTags  <|>
            tryWith (\p -> Braced  <$> p <*>
             (char '{' *> many oneLiner <* char '}'))          bracedTags <|>
            tryWith (\p -> Math    <$> (p *> mathArg "{}\n"))  mathTags   <|>
            tryWith (\p -> Comment <$> (p *> notEOL))          commentTags)

notEOL :: Parser Text
notEOL = option "" $ many1 (char ' ') *> many (noneOf "\n")

lineArg :: Parser Texinfo
lineArg = void `orMany` oneLiner

-- Comment line argument should be left as it is
-------------------------------------------------
commentArg :: Parser Text
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
singleTags = map (:[]) "-/`^\"{}@*'|," ++ words "short thispage"

emptyTags :: [Tag]
emptyTags = words "TeX copyright dots"

bracedTags :: [Tag]
bracedTags = words "acronym anchor b cite code dfn emph file footnote i image newterm r ref strong t titlefont url value var w"

mathTags :: [Tag]
mathTags = words "math"

lineTags :: [Tag]
lineTags = words "author bullet bye center chapter cindex dircategory endpage everyheading finalout heading include item node noindent printindex section setfilename setshortcontentsaftertitlepage settitle set sp subsection subsubheading subsubsection subtitle title unnumbered"

commentTags :: [Tag]
commentTags = words "c comment"

envTags :: [Tag]
envTags = words "detailmenu direntry enumerate example float ifinfo iftex itemize lisp macro menu quotation smallexample smalllisp titlepage"

texTags :: [Tag]
texTags = words "tex"

-- Read the Texinfo source from file
-------------------------------------
main :: IO ()
main = do
  result <- parseFromFile texinfo "sicp.texi"
  writeFile "parsed-sicp.txt" $ show result
  --print result
