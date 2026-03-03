-- # Token
-- 字句解析
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}

module Token
    ( Lexeme
    , Loc
    , Token
    , clex
    ) where

import Control.Arrow
import Data.Char
import Data.List.Extra

{- $setup
>>> :set -XOverloadedStrings
-}

data Lexeme
    = Iden  String
    | Numb  Integer
    | Symb  String
    | Bksl
    | Quot  Char
    | Parn  Char
    | Ukwn  Char
    deriving (Eq, Show, Read)

type Loc   = (Int, Int)
type Token = (Lexeme, Loc)

{- |
>>> sampleS = "S = \\f.\\g.\\x.f x (g x);" :: String
>>> sampleK = "K = \\x.\\y.x;" :: String
>>> clex (1,1) sampleS
[(Iden "S",(1,1)),(Symb "=",(1,3)),(Bksl,(1,5)),(Iden "f",(1,6)),(Symb ".",(1,7)),(Bksl,(1,8)),(Iden "g",(1,9)),(Symb ".",(1,10)),(Bksl,(1,11)),(Iden "x",(1,12)),(Symb ".",(1,13)),(Iden "f",(1,14)),(Iden "x",(1,16)),(Parn '(',(1,18)),(Iden "g",(1,19)),(Iden "x",(1,21)),(Parn ')',(1,22)),(Symb ";",(1,23))]
>>> clex (1,1) sampleK
[(Iden "K",(1,1)),(Symb "=",(1,3)),(Bksl,(1,5)),(Iden "x",(1,6)),(Symb ".",(1,7)),(Bksl,(1,8)),(Iden "y",(1,9)),(Symb ".",(1,10)),(Iden "x",(1,11)),(Symb ";",(1,12))]
-}
clex :: Loc -> String -> [Token]
clex loc = \ case
    ccs@(c:cs)
        | c == '\n'
            -> clex (succ *** const 1 $ loc) cs
        | c == ' '
            -> clex (second succ loc) cs
        | c == '\\'
            -> (Bksl, loc) : clex (second succ loc) cs
        | isParen c
            -> (Parn c, loc) : clex (second succ loc) cs
        | isQuote c
            -> (Quot c, loc) : clex (second succ loc) cs
        | isNumbChar c
            -> case span isNumbChar ccs of
                (as,bs) -> (Numb (read as), loc) : clex (second (length as +) loc) bs
        | isIdenHead c
            -> case span isIdenTail cs of
                (as,bs) -> (Iden (c:as), loc) : clex (second (succ . (length as +)) loc) bs
        | isSymbChar c
            -> case span isSymbChar ccs of
                (as,bs) -> (Symb as, loc) : clex (second (length as +) loc) bs
        | otherwise
            -> (Ukwn c, loc) : clex (second succ loc) cs
    [] -> []

{- |
>>> sample1 = chr <$> [0 .. 127]
>>> filter isIdenHead sample1
"ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
>>> filter isIdenTail sample1
"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
>>> filter isNumbChar sample1
"0123456789"
>>> filter isParen sample1
"()[]{}"
>>> filter isQuote sample1
"\"'`"
>>> filter isSymbChar sample1
"!#$%&*+,-./:;<=>?@^|~"
-}

isQuote :: Char -> Bool
isQuote c = elem c ("\"'`" :: String)

isParen :: Char -> Bool
isParen c = elem c ("()[]{}" :: String)

isIdenHead :: Char -> Bool
isIdenHead c = isAscii c && ('_' == c || isLetter c)

isIdenTail :: Char -> Bool
isIdenTail c = isAscii c && ('_' == c || isAlphaNum c)

isNumbChar :: Char -> Bool
isNumbChar c = isAscii c && isDigit c

isSymbChar :: Char -> Bool
isSymbChar c = isAscii c
            && c /= '\\'
            && not (isSpace c)
            && not (isIdenTail c)
            && not (isParen c)
            && not (isQuote c)
            && (isPunctuation c || isSymbol c)
