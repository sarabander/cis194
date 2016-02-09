{-# OPTIONS_GHC -Wall #-}

{- Parser for a subset of Texinfo in sicp.texi
   ===========================================

   Copyleft 2016 Andres Raba,
   GNU General Public License, version 3. -}


module Main where

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)
import Data.List.Split (splitOn)

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
                  | Assign Variable Value
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
atClause = do
  _ <- char '@'
  tag <- tagParser
  argParser tag

tagParser :: Parser Tag
tagParser = many1 letter <|> (:[]) <$> oneOf singles

singles :: String
singles = "/-`^\"{}@*'|,"

argParser :: Tag -> Parser TexiFragment
argParser tag = case tagType tag of
  SingleTag  -> Single  <$> pure tag
  EmptyTag   -> NoArg   <$> pure tag <* string "{}"
  BracedTag  -> Braced  <$> pure tag <*> bracedArg
  MathTag    -> Math    <$> mathArg "{}"
  LineTag    -> Line    <$> pure tag <*> lineArg
  AssignTag  -> Assign  <$> variable <*> value
  CommentTag -> Comment <$> commentArg
  EnvTag     -> Env     <$> pure tag <*>
                (spaces *> (manyTill texiFragment $ endTag tag))
  TeXTag     -> TeX     <$> (spaces *> (manyTill anyChar $ endTag tag))
  UnknownTag -> pure Void <* parserFail ("unrecognized tag: " ++ tag)

data TagType = SingleTag
             | EmptyTag
             | BracedTag
             | MathTag
             | LineTag
             | AssignTag
             | CommentTag
             | EnvTag
             | TeXTag
             | UnknownTag

tagType :: Tag -> TagType
tagType tag | tag ∊ singleSet  = SingleTag
            | tag ∊ noArgSet   = EmptyTag
            | tag ∊ bracedSet  = BracedTag
            | tag ∊ mathSet    = MathTag
            | tag ∊ lineSet    = LineTag
            | tag ∊ assignSet  = AssignTag
            | tag ∊ commentSet = CommentTag
            | tag ∊ envSet     = EnvTag
            | tag ∊ texSet     = TeXTag
            | otherwise        = UnknownTag

(∊) :: Tag -> S.Set Tag -> Bool
(∊) = S.member

bracedArg :: Parser Texinfo
bracedArg = char '{' *> texinfo <* char '}'

mathArg :: ExcludedChars -> Parser Text
mathArg excl = char '{' *> simpleText excl <* char '}'

lineArg :: Parser Texinfo
lineArg = do
  rawLine <- many (oneOf " \t") >> tillCommentOrEOL
  let parsedLine = parse texinfo "line argument" rawLine
  case parsedLine of
   Right result -> return result
   Left err -> return [Void] <* (parserFail $ show err)

tillCommentOrEOL :: Parser Text
tillCommentOrEOL =
  manyTill anyChar $
  lookAhead (try (string "@c ") <|> try (string "@comment "))
  <|> string "\n"

-- Parsers for the assignment command (@set <var> <val>)
---------------------------------------------------------
variable :: Parser Variable
variable = spaces *> many1 (noneOf " ")

value :: Parser Value
value = spaces *> manyTill anyChar newline

-- Comment line argument should be left as it is
-------------------------------------------------
commentArg :: Parser Text
commentArg = emptyLine `orMany` noneOf "\n"

emptyLine :: Parser Text
emptyLine = manyTill space (newline <|> eof *> pure ' ')

orMany :: Parser [a] -> Parser a -> Parser [a]
vp `orMany` p = try vp <|> space *> {- spaces *> -} manyTill p newline
-- vp is void-parser or empty-line parser, p should match something else
infixl 3 `orMany`

void :: Parser Texinfo
void = emptyLine *> pure [Void]

-- End of environment
----------------------
endTag :: Tag -> Parser EndTag
endTag tag = try $ string "@end " <* spaces <* string tag {- <* emptyLine -}

-- Sets of tags in each category
---------------------------------
specialSymbols :: String
specialSymbols = "%$"

singleSet :: S.Set Tag
singleSet = S.fromList $ words "\" ' * , - / @ ^ ` short thispage { | }"

noArgSet :: S.Set Tag
noArgSet = S.fromList $ words "TeX copyright dots"

bracedSet :: S.Set Tag
bracedSet = S.fromList $ words "acronym anchor b cite code dfn emph file footnote i image newterm r ref strong t titlefont url value var w"

mathSet :: S.Set Tag
mathSet = S.fromList $ words "math"

lineSet :: S.Set Tag
lineSet = S.fromList $ words "author bullet bye center chapter cindex dircategory endpage everyheading finalout heading include item node noindent printindex section setfilename setshortcontentsaftertitlepage settitle sp subsection subsubheading subsubsection subtitle title unnumbered"

assignSet :: S.Set Tag
assignSet = S.fromList $ words "set"

commentSet :: S.Set Tag
commentSet = S.fromList $ words "c comment"

envSet :: S.Set Tag
envSet = S.fromList $ words "detailmenu direntry enumerate example float ifinfo iftex itemize lisp macro menu quotation smallexample smalllisp titlepage"

texSet :: S.Set Tag
texSet = S.fromList $ words "tex"

-- MAIN --
----------
main :: IO ()
main = do
  commandArgs <- getArgs -- extract command line arguments to a list
  case commandArgs of
   [] -> putStrLn "No input filename given." >>
         putStrLn "Usage: sicp-parser <input> [ <output> ]."
   (i:[]) -> translateFile i o -- If only input filename is given then
     where o = case reverse (splitOn "." i) of -- construct outfile name:
                ("texi":_:_) -> init i -- convert ".texi" to ".tex",
                _ -> i ++ ".tex"       -- otherwise just append ".tex".
   (i:o:_) -> translateFile i o -- Both infile and outfile are given.

-- Read the Texinfo source from file and run the parser over it
----------------------------------------------------------------
translateFile :: FilePath -> FilePath -> IO ()
translateFile inFile outFile = do
  parseTree <- parseFromFile texinfo inFile
  let toPair = \(Assign var val) -> (var, val)
  let dictionary = either (const M.empty) id $
                   fmap (M.fromList . map toPair . filter isAssign) $
                   parseTree
  let translated = either show id $
                   fmap (trTexinfo ("global", dictionary)) $
                   parseTree
  writeFile "parsetree.txt" $ show parseTree
  writeFile outFile translated  -- Latex
  print $ M.toList dictionary

isAssign :: TexiFragment -> Bool
isAssign (Assign _ _) = True
isAssign _ = False
                 
type Context = String  -- if we are inside @code{..}, then context is "code"
type Variable = String -- a variable defined with @set <variable> <value>
type Value = String    -- a value of the variable assigned with @set
type Environment = (Context, M.Map Variable Value)

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
  Assign _ _ -> ""
  Env tag texi -> trTexinfo (tag, snd e) texi
  TeX markup -> markup

line :: Tag -> LaTeX -> LaTeX
line tag arg = case tag of
  --"set" -> ""
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
