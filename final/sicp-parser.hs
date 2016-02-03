{-# OPTIONS_GHC -Wall #-}

{- Parser for a subset of Texinfo in sicp.texi
   ===========================================

   Copyleft 2016 Andres Raba,
   GNU General Public License, version 3. -}


module Main where

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Applicative
import qualified Data.Map as M

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
type LaTeX = String

data TexiFragment = Void
                  | Comment Text
                  | Plain Text
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
plainText = Plain <$> simpleText ("@{}" ++ specialSymbols)

simpleText :: ExcludedChars -> Parser Text
simpleText excl = concat <$>
                  many1 (nestedBraces excl <|> many1 (noneOf excl))

nestedBraces :: ExcludedChars -> Parser Text
nestedBraces excl = try (string "{}") <|>
                    (\x y z -> x ++ y ++ z) <$>
                    string "{" <*> simpleText excl <*> string "}"

-- Special symbol
------------------
special :: Parser TexiFragment
special = (Special . (:[])) <$> oneOf specialSymbols

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
oneLiner = Plain <$> simpleText ("@{}\n" ++ specialSymbols)               <|>
           special                                                        <|>
           (char '@') *>
           (tryWith (\p -> Single  <$> p)                      singleTags <|>
            tryWith (\p -> NoArg   <$> p <* string "{}")       emptyTags  <|>
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
specialSymbols :: String
specialSymbols = "%$"

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
main = do
  parseTree <- parseFromFile texinfo "sicp.texi"
  let onlySet = filter isSetCommand $
                either (const [Plain "error"]) id parseTree
  let setArg = map (\l -> case l of
                              (Line _ (Plain arg : _)) -> arg
                              _ -> "seterror 0") onlySet
  let eitherPairs = map (parse assignment "set arg") setArg
  let pairs = map (either (const ("seterror","1")) id) eitherPairs
  let dictionary = M.fromList pairs
  let translated = either show id $
                   fmap (trTexinfo ("global", dictionary)) parseTree
  writeFile "parsed-sicp.txt" $ translated  -- Latex
  --writeFile "parsed-sicp.txt" $ show parseTree
  print dictionary

isSetCommand :: TexiFragment -> Bool
isSetCommand (Line "set" _) = True
isSetCommand _ = False

type Context = String  -- if we are inside @code{..}, then context is "code"
type Variable = String -- a variable defined with @set <variable> <value>
type Value = String    -- a value of the variable assigned with @set
type Environment = (Context, M.Map Variable Value)

assignment :: Parser (Variable, Value)
assignment = (,) <$> (spaces *> many1 (noneOf " ")) <*>
             (spaces *> (many anyChar))

-- Translate the Texinfo parse tree to LaTeX
---------------------------------------------
trTexinfo :: Environment -> Texinfo -> LaTeX
trTexinfo e = concat . map (trTexiFragment e)

trTexiFragment :: Environment -> TexiFragment -> LaTeX
trTexiFragment e fr = case fr of
  Void -> ""
  Comment text -> "% " ++ text ++ "\n"
  Plain text -> text
  Special symbol -> "\\" ++ symbol
  Single tag -> single tag
  NoArg tag -> noArg tag
  Braced tag texi -> braced e tag $ trTexinfo (tag, snd e) texi
  Math expr -> inlineMath (fst e) expr
  Line tag texi -> line tag $ trTexinfo (tag, snd e) texi
  Env tag texi -> trTexinfo (tag, snd e) texi
  TeX markup -> markup

line :: Tag -> LaTeX -> LaTeX
line tag arg = case tag of
  "set" -> ""
  _ -> arg ++ "\n"

single :: Tag -> LaTeX
single tag = case tag of
  "-"  -> ""
  "/"  -> ""
  "|"  -> ""
  "`"  -> "\\`"
  "^"  -> "\\^"
  "\"" -> "\\\""
  "*"  -> "\\\\"
  "'"  -> "\\'"
  ","  -> "\\c"
  "thispage" -> ""
  "short" -> "\\short"
  _ -> tag

noArg :: Tag -> LaTeX
noArg "dots" = "\\( \\dots \\)"
noArg tag = "{\\" ++ tag ++ "}"

braced :: Environment -> Tag -> LaTeX -> LaTeX
braced e tag arg = case tag of
  "anchor" -> glue "label" arg
  "b" -> glue "textbf" arg
  "cite" -> glue "textit" arg
  "file" -> glue "texttt" arg
  "i" -> glue "textit" arg
  "r" -> glue "textrm" arg
  "ref" -> glue "link" arg
  "strong" -> glue "heading" arg
  "t" -> glue "texttt" arg
  "w" -> glue "mbox" arg
  "dfn" -> "% " ++ arg
  "titlefont" -> "% " ++ arg
  "image" -> image arg
  "value" -> maybe ("\\undefined_variable{" ++ arg ++ "}") id $
             M.lookup arg (snd e)
  _ -> glue tag arg

glue :: String -> LaTeX -> LaTeX
glue latexTag latexArg = "\\" ++ latexTag ++ "{" ++ latexArg ++ "}"

inlineMath :: Context -> Expression -> LaTeX
inlineMath context expr =
  "\\( " ++
  (if context == "lisp" then "\\dark" else "") ++
  expr ++
  " \\)"

type File = String
type Width = String
type Height = String
type Alt = String
type Ext = String

data Image = Img File Width Height Alt Ext deriving Show

imageArg :: Parser Image
imageArg = Img <$>
           (argPart <* comma) <*>
           (argPart <* comma) <*>
           (argPart <* comma) <*>
           (altText <* comma) <*>
           argPart

argPart, altText :: Parser String
argPart = spaces *> many (noneOf ", ") <* spaces
altText = spaces *> many (noneOf ",")

comma :: Parser Char
comma = char ','

image :: String -> LaTeX
image arg = "\\includegraphics[" ++ dimension ++ "]{" ++ filename ++ "}"
  where imgParts = parse imageArg "@image argument" arg
        extract (Right img) = img
        extract (Left _) = (Img "Image parse error" "" "" "" "")
        (Img file width height _ ext) = extract imgParts
        filename = file ++ ext
        dimension = makeDim width height
        makeDim "" "" = ""
        makeDim "" h = "height=" ++ h
        makeDim w _ = "width=" ++ w

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef

braces :: Parser a -> Parser a
braces = P.braces lexer
