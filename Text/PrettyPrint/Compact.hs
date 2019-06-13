{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Compact pretty-printer.
--
-- == Examples
--
-- Assume that we want to pretty print S-Expressions, which can either be atom or a list of S-Expressions.
--
-- >>> data SExpr = SExpr [SExpr] | Atom String deriving Show
-- >>> let pretty :: SExpr -> Doc (); pretty (Atom s) = text s; pretty (SExpr xs) = text "(" <> (sep $ map pretty xs) <> text ")"
--
-- Using the above representation, the S-Expression @(a b c d)@ has the following encoding:
--
-- >>> let abcd = SExpr [Atom "a",Atom "b",Atom "c",Atom "d"]
--
-- The legible layouts of the @abcd@ S-Expression defined above would be either
--
-- >>> putStrLn $ render $ pretty abcd
-- (a b c d)
--
-- or
--
-- >>> putStrLn $ renderWith defaultOptions { optsPageWidth = 5 } $ pretty abcd
-- (a
--  b
--  c
--  d)
--
-- The @testData@ S-Expression is specially crafted to
-- demonstrate general shortcomings of both Hughes and Wadler libraries.
--
-- >>> let abcd4 = SExpr [abcd,abcd,abcd,abcd]
-- >>> let testData = SExpr [ SExpr [Atom "abcde", abcd4], SExpr [Atom "abcdefgh", abcd4]]
-- >>> putStrLn $ render $ pretty testData
-- ((abcde ((a b c d) (a b c d) (a b c d) (a b c d)))
--  (abcdefgh ((a b c d) (a b c d) (a b c d) (a b c d))))
--
-- on 20-column-wide page
--
-- >>> putStrLn $ renderWith defaultOptions { optsPageWidth = 20 } $ pretty testData
-- ((abcde ((a b c d)
--          (a b c d)
--          (a b c d)
--          (a b c d)))
--  (abcdefgh
--   ((a b c d)
--    (a b c d)
--    (a b c d)
--    (a b c d))))
--
-- Yet, neither Hughes' nor Wadler's library can deliver those results.
--
-- === Annotations
--
-- For example we can annotate every /car/ element of S-Expressions,
-- and in the rendering phase emphasise them by rendering them in uppercase.
--
-- >>> let pretty' :: SExpr -> Doc Any; pretty' (Atom s) = text s; pretty' (SExpr []) = text "()"; pretty' (SExpr (x:xs)) = text "(" <> (sep $ annotate (Any True) (pretty' x) : map pretty' xs) <> text ")"
-- >>> let render' = renderWith defaultOptions { optsAnnotate  = \a x -> if a == Any True then map toUpper x else x }
-- >>> putStrLn $ render' $ pretty' testData
-- ((ABCDE ((A B C D) (A B C D) (A B C D) (A B C D)))
--  (ABCDEFGH ((A B C D) (A b c d) (A b c d) (A b c d))))
--
module Text.PrettyPrint.Compact (
   -- * Documents
   ODoc,
   Doc,
   LM,
   Renderable,

   -- * Basic combinators
   module Data.Monoid, text, flush, char,

   hang, hangWith, encloseSep, list, tupled, semiBraces,

   -- * Operators
   (<+>), ($$), (</>), (<//>), (<$$>),

   -- * List combinators
   hsep, sep, hcat, vcat, cat, punctuate,

   -- * Fill combiantors
   -- fillSep, fillCat,

   -- * Bracketing combinators
   enclose, squotes, dquotes, parens, angles, braces, brackets,

   -- * Character documents
   lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
   squote, dquote, semi, colon, comma, space, dot, backslash, equals,

   -- * Primitive type documents
   string, int, integer, float, double, rational,
   bool,

   -- * Rendering
   renderWith,
   render,
   Options(..),
   defaultOptions,

   -- * Annotations
   annotate,

   -- * Undocumented
   -- column, nesting, width
   ) where

import Data.Monoid

import Text.PrettyPrint.Compact.Core as Text.PrettyPrint.Compact

-- | Render the 'Doc' into 'String' omitting all annotations.
render :: (Renderable d a, Annotation a) => d a -> String
render = renderWith defaultOptions

defaultOptions :: Options a String
defaultOptions = Options
    { optsAnnotate = \_ s -> s
    , optsPageWidth = 80
    }

-- | The document @(list xs)@ comma separates the documents @xs@ and
-- encloses them in square brackets. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma separators are put in front of the elements.
list :: (Document d a b, Annotation a) => [ODoc d a] -> ODoc d a
list            = encloseSep lbracket rbracket comma

-- | The document @(tupled xs)@ comma separates the documents @xs@ and
-- encloses them in parenthesis. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma separators are put in front of the elements.
tupled :: (Document d a b, Annotation a) => [ODoc d a] -> ODoc d a
tupled          = encloseSep lparen   rparen  comma


-- | The document @(semiBraces xs)@ separates the documents @xs@ with
-- semi colons and encloses them in braces. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All semi colons are put in front of the elements.
semiBraces :: (Document d a b, Annotation a) => [ODoc d a] -> ODoc d a
semiBraces      = encloseSep lbrace   rbrace  semi

-- | The document @(enclosure l r sep xs)@ concatenates the documents
-- @xs@ separated by @sep@ and encloses the resulting document by @l@
-- and @r@. The documents are rendered horizontally if that fits the
-- page. Otherwise they are aligned vertically. All separators are put
-- in front of the elements. For example, the combinator 'list' can be
-- defined with @enclosure@:
--
-- > list xs = enclosure lbracket rbracket comma xs
-- > test    = text "list" <+> (list (map int [10,200,3000]))
--
-- Which is layed out with a page width of 20 as:
--
-- @
-- list [10,200,3000]
-- @
--
-- But when the page width is 15, it is layed out as:
--
-- @
-- list [10
--      ,200
--      ,3000]
-- @
encloseSep :: (Document d a b, Annotation a) =>
  ODoc d a -> ODoc d a -> ODoc d a -> [ODoc d a] -> ODoc d a
encloseSep left right separator ds
    = (<> right) $ case ds of
        []  -> left
        [d] -> left <> d
        (d:ds') -> cat (left <> d:map (separator <>) ds')

-----------------------------------------------------------
-- punctuate p [d1,d2,...,dn] => [d1 <> p,d2 <> p, ... ,dn]
-----------------------------------------------------------


-- | @(punctuate p xs)@ concatenates all documents in @xs@ with
-- document @p@ except for the last document.
--
-- > someText = map text ["words","in","a","tuple"]
-- > test     = parens (align (cat (punctuate comma someText)))
--
-- This is layed out on a page width of 20 as:
--
-- @
-- (words,in,a,tuple)
-- @
--
-- But when the page width is 15, it is layed out as:
--
-- @
-- (words,
--  in,
--  a,
--  tuple)
-- @
--
-- (If you want put the commas in front of their elements instead of
-- at the end, you should use 'tupled' or, in general, 'encloseSep'.)
punctuate :: (Layout d, Semigroup (d a), Annotation a) =>
             d a -> [d a] -> [d a]
punctuate _p []      = []
punctuate _p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds


-----------------------------------------------------------
-- high-level combinators
-----------------------------------------------------------


-- | The document @(sep xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<+\>)@, if it fits the page, or vertically
-- with @(\<$$\>)@. Documents on the left of horizontal concatenation
-- must fit on a single line.
--
sep :: (Document d a b, Annotation a) => [ODoc d a] -> ODoc d a
sep xs = groupingBy " " (map (0,) xs)


-- -- | The document @(fillSep xs)@ concatenates documents @xs@
-- -- horizontally with @(\<+\>)@ as long as its fits the page, than
-- -- inserts a @line@ and continues doing that for all documents in
-- -- @xs@.
-- --
-- -- > fillSep xs  = foldr (\<\/\>) empty xs
-- fillSep :: (Layout d, Annotation a) => [ODoc d a] -> ODoc d a
-- fillSep         = foldDoc (</>)

-- | The document @(hsep xs)@ concatenates all documents @xs@
-- horizontally with @(\<+\>)@.
hsep :: (Layout d, Monoid (d a), Annotation a) => [d a] -> d a
hsep            = foldDoc (<+>)

-- | The document @(cat xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<\>)@, if it fits the page, or vertically with
-- @(\<$$\>)@.
--
cat :: (Document d a b, Annotation a) => [ODoc d a] -> ODoc d a
cat xs = groupingBy "" (map (0,) xs)

-- -- | The document @(fillCat xs)@ concatenates documents @xs@
-- -- horizontally with @(\<\>)@ as long as its fits the page, than inserts
-- -- a @linebreak@ and continues doing that for all documents in @xs@.
-- --
-- -- > fillCat xs  = foldr (\<\/\/\>) empty xs
-- fillCat :: (Layout d, Annotation a) => [ODoc d a] -> ODoc d a
-- fillCat         = foldDoc (<//>)

-- | The document @(hcat xs)@ concatenates all documents @xs@
-- horizontally with @(\<\>)@.
hcat :: (Layout d, Monoid (d a), Annotation a) => [d a] -> d a
hcat            = foldDoc (<>)

-- | The document @(vcat xs)@ concatenates all documents @xs@
-- vertically with @($$)@.
vcat :: (Layout d, Monoid (d a), Annotation a) => [d a] -> d a
vcat            = foldDoc ($$)

foldDoc :: (Layout d, Monoid (d a), Annotation a) => (d a -> d a -> d a) -> [d a] -> d a
foldDoc _ []       = mempty
foldDoc f ds       = foldr1 f ds

-- | The document @(x \<+\> y)@ concatenates document @x@ and @y@ with a
-- @space@ in between.  (infixr 6)
(<+>) :: (Layout d, Semigroup (d a), Annotation a) => d a -> d a -> d a
x <+> y         = x <> space <> y

-- | The document @(x \<\/\> y)@ puts @x@ and @y@ either next to each other
-- (with a @space@ in between) or underneath each other. (infixr 5)
(</>) :: (Document d a b, Annotation a) => ODoc d a -> ODoc d a -> ODoc d a
x </> y         = hang 0 x y

-- | The document @(x \<\/\/\> y)@ puts @x@ and @y@ either right next
-- to each other (if @x@ fits on a single line) or underneath each
-- other. (infixr 5)
(<//>) :: (Document d a b, Annotation a) => ODoc d a -> ODoc d a -> ODoc d a
x <//> y        = hangWith "" 0 x y

-- | The document @(x \<$$\> y)@ concatenates document @x@ and @y@ with
-- a linebreak in between. (infixr 5)
(<$$>) :: (Layout d, Semigroup (d a), Annotation a) => d a -> d a -> d a
(<$$>) = ($$)

-- | Document @(squotes x)@ encloses document @x@ with single quotes
-- \"'\".
squotes :: (Document d a b, Semigroup (d a), Annotation a) => d a -> d a
squotes         = enclose squote squote

-- | Document @(dquotes x)@ encloses document @x@ with double quotes
-- '\"'.
dquotes :: (Document d a b, Semigroup (d a), Annotation a) => d a -> d a
dquotes         = enclose dquote dquote

-- | Document @(braces x)@ encloses document @x@ in braces, \"{\" and
-- \"}\".
braces :: (Document d a b, Semigroup (d a), Annotation a) => d a -> d a
braces          = enclose lbrace rbrace

-- | Document @(parens x)@ encloses document @x@ in parenthesis, \"(\"
-- and \")\".
parens :: (Document d a b, Semigroup (d a), Annotation a) => d a -> d a
parens          = enclose lparen rparen

-- | Document @(angles x)@ encloses document @x@ in angles, \"\<\" and
-- \"\>\".
angles :: (Document d a b, Semigroup (d a), Annotation a) => d a -> d a
angles          = enclose langle rangle

-- | Document @(brackets x)@ encloses document @x@ in square brackets,
-- \"[\" and \"]\".
brackets :: (Document d a b, Semigroup (d a), Annotation a) => d a -> d a
brackets        = enclose lbracket rbracket

-- | The document @(enclose l r x)@ encloses document @x@ between
-- documents @l@ and @r@ using @(\<\>)@.
enclose :: (Layout d, Semigroup (d a), Annotation a) =>
           d a -> d a -> d a -> d a
enclose l r x   = l <> x <> r

char :: (Layout d, Annotation a) => Char -> d a
char x = text [x]

-- | The document @lparen@ contains a left parenthesis, \"(\".
lparen :: (Layout d, Annotation a) => d a
lparen          = char '('
-- | The document @rparen@ contains a right parenthesis, \")\".
rparen :: (Layout d, Annotation a) => d a
rparen          = char ')'
-- | The document @langle@ contains a left angle, \"\<\".
langle :: (Layout d, Annotation a) => d a
langle          = char '<'
-- | The document @rangle@ contains a right angle, \">\".
rangle :: (Layout d, Annotation a) => d a
rangle          = char '>'
-- | The document @lbrace@ contains a left brace, \"{\".
lbrace :: (Layout d, Annotation a) => d a
lbrace          = char '{'
-- | The document @rbrace@ contains a right brace, \"}\".
rbrace :: (Layout d, Annotation a) => d a
rbrace          = char '}'
-- | The document @lbracket@ contains a left square bracket, \"[\".
lbracket :: (Layout d, Annotation a) => d a
lbracket        = char '['
-- | The document @rbracket@ contains a right square bracket, \"]\".
rbracket :: (Layout d, Annotation a) => d a
rbracket        = char ']'


-- | The document @squote@ contains a single quote, \"'\".
squote :: (Layout d, Annotation a) => d a
squote          = char '\''
-- | The document @dquote@ contains a double quote, '\"'.
dquote :: (Layout d, Annotation a) => d a
dquote          = char '"'
-- | The document @semi@ contains a semi colon, \";\".
semi :: (Layout d, Annotation a) => d a
semi            = char ';'
-- | The document @colon@ contains a colon, \":\".
colon :: (Layout d, Annotation a) => d a
colon           = char ':'
-- | The document @comma@ contains a comma, \",\".
comma :: (Layout d, Annotation a) => d a
comma           = char ','

-- | The document @dot@ contains a single dot, \".\".
dot :: (Layout d, Annotation a) => d a
dot             = char '.'
-- | The document @backslash@ contains a back slash, \"\\\".
backslash :: (Layout d, Annotation a) => d a
backslash       = char '\\'
-- | The document @equals@ contains an equal sign, \"=\".
equals :: (Layout d, Annotation a) => d a
equals          = char '='

-----------------------------------------------------------
-- Combinators for prelude types
-----------------------------------------------------------

-- string is like "text" but replaces '\n' by "line"

-- | The document @(string s)@ concatenates all characters in @s@
-- using @line@ for newline characters and @char@ for all other
-- characters. It is used instead of 'text' whenever the text contains
-- newline characters.
string :: (Layout d, Monoid (d a), Annotation a) => String -> d a
string = vcat . map text . lines

bool :: (Layout d, Annotation a) => Bool -> d a
bool b          = text (show b)

-- | The document @(int i)@ shows the literal integer @i@ using
-- 'text'.
int :: (Layout d, Annotation a) => Int -> d a
int i           = text (show i)

-- | The document @(integer i)@ shows the literal integer @i@ using
-- 'text'.
integer :: (Layout d, Annotation a) => Integer -> d a
integer i       = text (show i)

-- | The document @(float f)@ shows the literal float @f@ using
-- 'text'.
float :: (Layout d, Annotation a) => Float -> d a
float f         = text (show f)

-- | The document @(double d)@ shows the literal double @d@ using
-- 'text'.
double :: (Layout d, Annotation a) => Double -> d a
double d        = text (show d)

-- | The document @(rational r)@ shows the literal rational @r@ using
-- 'text'.
rational :: (Layout d, Annotation a) => Rational -> d a
rational r      = text (show r)



-- | The hang combinator implements hanging indentation. The document
-- @(hang i x y)@ either @x@ and @y@ concatenated with @\<+\>@ or @y@
-- below @x@ with an additional indentation of @i@.

hang :: (Document d a b, Annotation a) => Int -> ODoc d a -> ODoc d a -> ODoc d a
hang = hangWith " "


-- | The hang combinator implements hanging indentation. The document
-- @(hang separator i x y)@ either @x@ and @y@ concatenated with @\<\>
-- text separator \<\>@ or @y@ below @x@ with an additional
-- indentation of @i@.
hangWith :: (Document d a b, Annotation a) =>
            String -> Int -> ODoc d a -> ODoc d a -> ODoc d a
hangWith separator n x y = groupingBy separator [(0,x), (n,y)]

space :: (Layout d, Annotation a) => d a
space = text " "

-- $setup
-- >>> import Data.Monoid
-- >>> import Data.Char
