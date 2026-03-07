-- # Core.Pretty
-- コア言語のプリティプリンタ
-- 
-- ## 言語拡張と`module`宣言
{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
module Core.Pretty
    where

import Control.Arrow hiding ((<+>))
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable

import Data.Char
import Data.Bool
import Data.List ( dropWhileEnd, isPrefixOf )
import Data.Maybe
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Util

import Core.Language
import Core.Bop

pprProgram :: forall a. VarRep a
           => forall ann. (a -> Doc ann)
           -> Program a -> Doc ann
pprProgram ppr 
    = align
    . vsep
    . punctuate semi
    . map (pprScDefn ppr)

pprCoreProgram :: CoreProgram -> Doc ann
pprCoreProgram = pprProgram pprCore 

pprScDefn :: forall a. VarRep a
          => forall ann. (a -> Doc ann)
          -> ScDefn a -> Doc ann
pprScDefn ppr = \ case
    (name,xs,e) -> group (pretty name <> pprArgs ppr xs)
               <+> "=" <+> pprExpr ppr e

pprArgs :: (a -> Doc ann) -> [a] -> Doc ann
pprArgs ppr = \ case
    [] -> mempty
    xs -> space <> hsep (map ppr xs)

pprExpr :: forall a. VarRep a 
        => forall ann. (a -> Doc ann) -> Expr a -> Doc ann
pprExpr ppr = snd . histoExpr phi where
    phi = \ case
        EVarF v      -> (maxBound, pretty v)
        ENumF n      -> (maxBound, pretty n)
        EConstrF t a -> (maxBound, "Pack" <> braces (surround comma (pretty t) (pretty a)))
        EApF e1 e2 -> case e1 of
            (_,_) :< EApF e11 e12 -> case e11 of
                (_,d11) :< EVarF bop
                    | isBop bop -> case bopInfoOf bop of
                        (p,Infix)   -> case e12 of
                            (o1,do1) :< _ -> case e2 of
                                (o2,do2) :< _
                                    -> (p, doc) where
                                        doc = parens' (p >= o1) do1
                                           <> softline <> d11 <+> parens' (p >= o2) do2
                        (p,InfixL)  -> case e12 of
                            (o1,do1) :< _ -> case e2 of
                                (o2,do2) :< _
                                    -> (p, doc) where
                                        doc = parens' (p > o1) do1
                                           <> softline <> d11 <+> parens' (p >= o2) do2
                        (p,InfixR)  -> case e12 of
                            (o1,do1) :< _ -> case e2 of
                                (o2,do2) :< _
                                    -> (p, doc) where
                                        doc = parens' (p >= o1) do1
                                           <> softline <> d11 <+> parens' (p > o2) do2
                    | otherwise -> pprEAp e1 e2
                _ -> pprEAp e1 e2
            _ -> pprEAp e1 e2
        ELetF bs e   -> (minBound, doc) where
            doc = "let" <> hardline 
                <> indent 4 bs' <> hardline
                <> indent -4 "in" <> hardline
                <> indent 4 e'
            bs' = pprBinders bs
            e'  = case e of
                (_,d) :< _ -> d
        ECaseF e alts -> (minBound, doc) where
            doc = "case" <+> e' <+> "of" <> hardline
               <> indent 4 alts'
            e' = case e of
                (_,d) :< _ -> d
            alts' = pprAlts alts
        ELamF xs body -> case sectionTypeOf xs body of
            NotSection           -> pprELam xs body
            SectionBoth bop      -> (maxBound, bop') where
                bop' = pprBop bop
            SectionL bop info (p,d1) -> case info of                 -- (x +)
                (o,Infix)  -> (maxBound, parens doc) where
                    doc = parens' (o >= p) d1 <+> pretty bop
                (o,InfixL) -> (maxBound, parens doc) where
                    doc = parens' (o >= p) d1 <+> pretty bop
                (o,InfixR) -> (maxBound, parens doc) where
                    doc = parens' (o >  p) d1 <+> pretty bop
            SectionR bop info (q,d2) -> case info of                 -- (+ y)
                (o,Infix)  -> (maxBound, parens doc) where
                    doc = pretty bop <+> parens' (o >= q) d2
                (o,InfixL) -> (maxBound, parens doc) where
                    doc = pretty bop <+> parens' (o >  q) d2
                (o,InfixR) -> (maxBound, parens doc) where
                    doc = pretty bop <+> parens' (o >= q) d2

    pprEAp e1 e2 =  case (snd *** snd) (pprFun e1, pprArg e2) of
        (d1,d2) -> (10 :: Int, hang 4 (d1 <> softline <> d2))

    pprFun = \ case
        (p,doc) :< _ -> (bool maxBound p (p == 10), parens' (p < 10) doc)

    pprArg = \ case
        (p,doc) :< _ -> (undefined, parens' (p < maxBound) doc)

    pprBinders
        = align
        . vsep
        . punctuate semi
        . map (\ (a,(_,d):<_) -> group (ppr a <+> "=" <+> d))

    pprAlts
        = align
        . vsep
        . punctuate semi
        . map (\ (t,xs,(_,d) :< _) -> angles (pretty t)
                                <> pprArgs ppr xs
                                <+> "→" <+> d)
    
    pprELam xs body = (minBound, doc) where
        doc = case body of
            (_,body') :< _ -> group ("λ" <> pprArgs ppr xs <+> "→") <> softline <> body'

data SectionType d
    = NotSection
    | SectionBoth Name    -- (+)
    | SectionL Name BopInfo d  -- (x +)
    | SectionR Name BopInfo d  -- (+ y)

sectionTypeOf :: VarRep a => [a] -> AnnExpr a (Int, Doc ann) -> SectionType (Int, Doc ann)
sectionTypeOf xs body = case xs of
    [x] -> case body of
        _ :< EApF e1 e2 -> case e1 of
            _ :< EApF e11 e12 -> case e11 of
                _ :< EVarF bop
                    | isBop bop -> case e12 of
                        ae12 :< EVarF y -> case e2 of
                            ae2 :< EVarF z
                                | y == z       -> NotSection
                                | vname x == y -> SectionR bop (bopInfoOf bop) ae2
                                | vname x == z -> SectionL bop (bopInfoOf bop) ae12
                                | otherwise    -> NotSection
                            ae2 :< _ 
                                | vname x == y -> SectionR bop (bopInfoOf bop) ae2
                                | otherwise    -> NotSection
                        ae12 :< _       -> case e2 of
                            _ :< EVarF z
                                | vname x == z -> SectionL bop (bopInfoOf bop) ae12
                                | otherwise    -> NotSection
                            _                  -> NotSection
                _               -> NotSection
            _                 -> NotSection
        _               -> NotSection
    [x,y] -> case body of
        _ :< EApF e1 e2 -> case e2 of
            _ :< EVarF w
                | vname y /= w  -> NotSection
                | otherwise     -> case e1 of
                    _ :< EApF e11 e12 -> case e11 of
                        _ :< EVarF bop
                            | isBop bop   -> case e12 of
                                _ :< EVarF z  
                                    | vname x == z -> SectionBoth bop
                                _                  -> NotSection
                        _                 -> NotSection
                    _                 -> NotSection
            _               -> NotSection
        _               -> NotSection
    _   -> NotSection

parens' :: Bool -> Doc ann -> Doc ann
parens' = \ case
    False -> id
    _     -> parens

pprBop :: String -> Doc ann
pprBop = \ case
    bop | "`" `isPrefixOf` bop -> pretty (trim bop)
        | otherwise            -> parens (pretty bop)
    where
        trim = dropWhileEnd ('`' ==) . dropWhile ('`' ==)

pprCore :: Name -> Doc ann
pprCore = pretty

{- |
>>> vsep $ pprExpr pprCore . sample <$> [1 .. 18]
x
7
Pack{2,2}
f (g x)
f g x
2 + 3
(2 + 3) * 5
(2 + 3) * 5 - 7
let
    x = 1;
    xs = Pack{1,0}
in
    Pack{2,2} x xs
case f (g x) of
    <1>  → 0;
    <2> h hs → succ (length hs)
λ f g x → case f (g x) of
    <1>  → 0;
    <2> h hs → succ (length hs)
(+ y)
(x +)
(+)
mod
λ x z → x + y
λ z y → x + y
λ z w → x + y

-}
sample :: Int -> CoreExpr
sample i = fromMaybe (ENum 404) (lookup i sampleExprs)

sampleExprs :: [(Int, CoreExpr)]
sampleExprs
    = zip [1 ..]
    [ EVar "x"
    , ENum 7
    , EConstr 2 2
    , EAp (EVar "f") (EAp (EVar "g") (EVar "x"))
    , EAp (EAp (EVar "f") (EVar "g")) (EVar "x")
    , EAp (EAp (EVar "+") (ENum 2)) (ENum 3)       -- 2 + 3
    , EAp (EAp (EVar "*") (sample 6)) (ENum 5)     -- (2 + 3) * 5
    , EAp (EAp (EVar "-") (sample 7)) (ENum 7)     -- (2 + 3) * 5 - 7
    , ELet [("x",ENum 1),("xs", EConstr 1 0)] (EAp (EAp (EConstr 2 2) (EVar "x")) (EVar "xs"))
    , ECase (sample 4) [(1,[],ENum 0),(2,["h","hs"],EAp (EVar "succ") (EAp (EVar "length") (EVar "hs")))]
    , ELam ["f","g","x"] (sample 10)
    , ELam ["x"] (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
    , ELam ["y"] (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
    , ELam ["x","y"] (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
    , ELam ["x","y"] (EAp (EAp (EVar "`mod`") (EVar "x")) (EVar "y"))
    , ELam ["x","z"] (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
    , ELam ["z","y"] (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
    , ELam ["z","w"] (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
    ]

sampleProgram :: CoreProgram
sampleProgram
    = [("main",[], EAp (EVar f) (foldr phi (EConstr 1 0) [1,2,3]))
      ,(f,xs,rhs)
      ,(g,[],rhs')]
    where
        phi x = EAp (EAp (EVar ":") (ENum x))
        f   = "length"
        xs  = ["xs"]
        rhs = ECase (EVar (head xs)) [(1,[],ENum 0),(2,["h","hs"],EAp (EVar g) (EAp (EVar f) (EVar "hs")))]
        g   = "succ"
        rhs' = ELam ["x"] (EAp (EAp (EVar "+") (ENum 1)) (EVar "x"))

{- ^
>>> pprCoreProgram sampleProgram
main = length (1 : 2 : 3 : Pack{1,0});
length xs = case xs of
    <1> → 0;
    <2> h hs → succ (length hs);
succ = (1 +)
-}
