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
import Data.Maybe (isJust, isNothing)
import Data.Char (isDigit)

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

data TexiFragment = Comment Text
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

argParser :: Tag -> Parser TexiFragment
argParser tag = case tagType tag of
  SingleTag  -> Single  <$> pure tag
  EmptyTag   -> NoArg   <$> pure tag <* string "{}"
  BracedTag  -> Braced  <$> pure tag <*> bracedArg
  MathTag    -> Math    <$> mathArg "{}"
  LineTag    -> Line    <$> pure tag <*> lineArg
  AssignTag  -> Assign  <$> variable <*> value
  CommentTag -> Comment <$> commentArg
  EnvTag     -> Env     <$> pure tag <*> (manyTill texiFragment $ endTag tag)
  TeXTag     -> TeX     <$>
                (newline *> (manyTill anyChar $ endTag tag) <* newline)
  UnknownTag -> parserFail ("unrecognized tag: " ++ tag)

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

-- Parsers for an argument inside braces
-----------------------------------------
bracedArg :: Parser Texinfo
bracedArg = char '{' *> texinfo <* char '}'

mathArg :: ExcludedChars -> Parser Text
mathArg excl = char '{' *> simpleText excl <* char '}'

-- Parsers for a single-line argument
--------------------------------------
lineArg :: Parser Texinfo
lineArg = do
  rawLine <- many (oneOf " \t") >> tillCommentOrEOL
  let parsedLine = parse texinfo "line argument" rawLine
  case parsedLine of
   Right result -> return result
   Left err -> parserFail $ show err

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

-- Recognize the end of environment
------------------------------------
endTag :: Tag -> Parser EndTag
endTag tag = try $ string "@end " <* spaces <* string tag {- <* emptyLine -}

-- Tags categorized by argument parsing style
----------------------------------------------
specialSymbols :: String
specialSymbols = "%$"

singles :: String
singles = "\"'*,-/@^`{|}"

singleSet :: S.Set Tag
singleSet = S.fromList $ map (:[]) singles ++ words "short thispage"

noArgSet :: S.Set Tag
noArgSet = S.fromList $ words "TeX copyright dots"

bracedSet :: S.Set Tag
bracedSet = S.fromList $ words "acronym anchor b caption cite code dfn emph file footnote i image newterm r ref strong t titlefont url value var w"

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
  --writeFile "parsetree.txt" $ show parseTree
  writeFile outFile translated  -- Latex
  --print $ M.toList dictionary

isAssign :: TexiFragment -> Bool
isAssign (Assign _ _) = True
isAssign _ = False

-- For remembering the command inside which we are and the assigned variables
------------------------------------------------------------------------------
type Context = String  -- if we are inside @code{..}, then context is "code"
type Variable = String -- a variable defined with @set <variable> <value>
type Value = String    -- a value of the variable assigned with @set
type Environment = (Context, M.Map Variable Value)

-- Mark LaTeX fragments in code listings with special sentinel and translate
-----------------------------------------------------------------------------
markFragment :: Environment -> TexiFragment -> LaTeX
markFragment e fr = case fr of
  Plain text -> text -- don't mark verbatim code
  _ -> sentinel ++ trTexiFrag e fr ++ sentinel

sentinel :: String
sentinel = "~"

-- Translate the Texinfo parse tree to LaTeX
---------------------------------------------
trTexinfo :: Environment -> Texinfo -> LaTeX
trTexinfo e@("lisp",_)      = concat . map (markFragment e)
trTexinfo e@("smalllisp",_) = concat . map (markFragment e)
trTexinfo e                 = concat . map (trTexiFrag e)

trTexiFrag :: Environment -> TexiFragment -> LaTeX
trTexiFrag e fr = case fr of
  Comment text     -> "% " ++ text ++ "\n"
  Plain text       -> text
  Special symbol   -> "\\" ++ symbol
  Single tag       -> single tag
  NoArg tag        -> noArg tag
  Braced tag texi  -> braced e tag $ trTexinfo (tag, snd e) texi
  Math expr        -> inlineMath (fst e) expr
  Line tag texi    -> line e tag $ trTexinfo (tag, snd e) texi
  Assign _ _       -> ""
  Env "float" texi -> figure e texi
  Env tag texi     -> env e tag $ trTexinfo (tag, snd e) texi
  TeX markup       -> markup

-- Helper functions that build LaTeX fragments
-----------------------------------------------
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
  --"short" -> "\\short"
  "short" -> ""
  _ -> tag

noArg :: Tag -> LaTeX
noArg "dots" = "\\( \\dots \\)"
noArg tag    = "{\\" ++ tag ++ "}"

braced :: Environment -> Tag -> LaTeX -> LaTeX
braced (c, d) tag arg = case tag of
  "anchor"    -> phantom c ++ glue "label" arg
  "b"         -> glue "textbf" arg
  "caption"   -> arg
  "cite"      -> glue "textit" arg
  "file"      -> glue "texttt" arg
  "i"         -> glue "textit" arg
  "r"         -> glue "textrm" arg
  "ref"       -> glue "link" $ prefixLink arg
  "strong"    -> glue "heading" arg
  "t"         -> glue "texttt" arg
  "w"         -> glue "mbox" arg
  "dfn"       -> "% " ++ arg
  "titlefont" -> "% " ++ arg
  "image"     -> image arg
  "var"       -> glue "var" $ (if c == "lisp" || c == "smalllisp"
                               then "\\dark " else "") ++ arg
  "value"     -> maybe ("\\undefined_variable{" ++ arg ++ "}") id $
                 M.lookup arg d
  _           -> glue tag arg

phantom :: Context -> LaTeX
phantom "float"  = "\\phantomsection"
phantom "strong" = "\\phantomsection"
phantom _ = ""

glue :: String -> LaTeX -> LaTeX
glue latexTag latexArg = "\\" ++ latexTag ++ "{" ++ latexArg ++ "}"

prefixLink :: LaTeX -> LaTeX
prefixLink arg = case arg of
  [] -> ""
  (x:_) -> if isDigit x then "Section " ++ arg else arg

inlineMath :: Context -> Expression -> LaTeX
inlineMath context expr =
  "\\( " ++
  (if context == "lisp" || context == "smalllisp"
   then "\\dark " else "") ++
  expr ++
  " \\)"

line :: Environment -> Tag -> LaTeX -> LaTeX
line (c,_) tag arg = case tag of
  "endpage"  -> ""
  "noindent" -> "\\noindent\n"
  "sp"       -> if c == "iftex" then ""
                else glue "vspace" (arg ++ "em") ++ "\n"
  _          -> arg ++ "\n"

env :: Environment -> Tag -> LaTeX -> LaTeX
env (c, _) tag arg = case tag of
  "example"      -> enclose "example" arg
  "smallexample" -> enclose "smallexample" arg
  "ifinfo"       -> enclose "comment" arg
  "macro"        -> enclose "comment" arg
  "titlepage"    -> enclose "comment" arg
  "quotation"    -> enclose "quote" arg
  "lisp"         -> if c == "footnote"
                    then enclose "smallscheme" arg
                    else enclose "scheme" arg
  "smalllisp"    -> enclose "smallscheme" arg
  _ -> arg

enclose :: String -> LaTeX -> LaTeX
enclose envr inner =
  "\\begin{" ++ envr ++ "}" ++ inner ++ "\\end{" ++ envr ++ "}"

-- Extract the structure of figure floats
------------------------------------------
figure :: Environment -> Texinfo -> LaTeX
figure _ [] = "" -- in case we had just '@float@end float'
figure (_, d) texi =
  maybe "\\error{Figure components are faulty or missing!}" id $
  do let get p = fmap (trTexiFrag ("float", d)) . seekTexi p
     let spot = get isPlacement [head texi] --must be right after @float
     place   <- if isNothing spot then Just "[tb]\n" else spot
     anchor  <- get isAnchor texi
     art     <- get isIfinfo texi
     istex   <- seekTexi isIftex texi -- get parse tree of @iftex
     img     <- get isImage [istex]
     caption <- get isCaption [istex]
     let title = if length caption < 69 || isJust (seekTexi isShort texi)
                 then "\\par\\bigskip\n\\noindent\n" ++ caption
                 else "\\begin{quote}\n" ++ caption ++ "\n\\end{quote}"
     return $
       "\\begin{figure}" ++ place ++ anchor ++ "\n\\centering\n"
       ++ art ++ "\n" ++ img ++ "\n" ++ title ++ "\n\\end{figure}"

-- Search a fragment that satisfies a predicate
------------------------------------------------
seekTexi :: (TexiFragment -> Bool) -> Texinfo -> Maybe TexiFragment
seekTexi _ [] = Nothing
seekTexi p (t:ts) = case seekFrag p t of
                     Nothing -> seekTexi p ts
                     result  -> result

seekFrag :: (TexiFragment -> Bool) -> TexiFragment -> Maybe TexiFragment
seekFrag p fr | p fr = Just fr
seekFrag p (Braced _ texi) = seekTexi p texi
seekFrag p (Line   _ texi) = seekTexi p texi
seekFrag p (Env    _ texi) = seekTexi p texi
seekFrag _ _ = Nothing

-- The predicates that match certain Texinfo fragments
-------------------------------------------------------
isPlacement, isAnchor, isIfinfo, isIftex, isImage, isCaption, isShort ::
  TexiFragment -> Bool
isPlacement (Plain ('[':_)) = True
isPlacement _ = False

isAnchor (Braced "anchor" _) = True
isAnchor _ = False

isIfinfo (Env "ifinfo" _) = True
isIfinfo _ = False

isIftex (Env "iftex" _) = True
isIftex _ = False

isImage (Braced "image" _) = True
isImage _ = False

isCaption (Braced "caption" _) = True
isCaption _ = False

isShort (Single "short") = True
isShort _ = False

-- Parse the components of an image
------------------------------------
data Image = Img File Width Height Alt Ext deriving Show

type File = String
type Width = String
type Height = String
type Alt = String
type Ext = String

image :: String -> LaTeX
image arg = "\\includegraphics[" ++ dimension ++ "]{" ++ filename ++ "}"
  where argParts = parse imageArg "image argument" arg
        (Img file width height _ ext) = case argParts of
          (Right img) -> img
          (Left _)    -> Img "Wrong argument format." "" "" "" ""
        filename = file ++ ext
        dimension = makeDim width height
        makeDim "" "" = ""
        makeDim "" h = "height=" ++ h
        makeDim w _ = "width=" ++ w

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
