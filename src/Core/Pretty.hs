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
module Core.Pretty
    where

import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable

import Data.Maybe
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Util

import Core.Language

pprExpr :: (a -> Doc ann) -> Expr a -> Doc ann
pprExpr ppr = paraExpr phi where
    phi = \ case
        EVarF v      -> pretty v
        ENumF n      -> pretty n
        EConstrF t v -> "Pack" <> braces (surround comma (pretty t) (pretty v))
        EApF e1 e2   -> pap1 e1 <+> pap2 e2
        ELetF bs body
            -> "let" <> hardline 
            <> indent 4 (align (vsep (punctuate semi (map (\ (a,r) -> group (ppr a <+> "=" <+> snd r)) bs)))) <> hardline
            <> indent -4 "in" <> hardline
            <> indent 4 (snd body)
        ECaseF e alts 
            -> "case" <+> (snd e) <+> "of" <> hardline
            <> indent 4 (vsep (punctuate semi (map (palt ppr) alts)))
        ELamF xs e -> group ("λ" <+> hsep (map ppr xs) <+> "→") <+> snd e


pap1 :: (Expr a, Doc ann) -> Doc ann
pap1 (e,d) = case e of
    ELet _ _  -> parens d
    ECase _ _ -> parens d
    ELam _ _  -> parens d
    _         -> d

pap2 :: (Expr a, Doc ann) -> Doc ann
pap2 (e,d) = case e of
    EVar _ -> d
    ENum _ -> d
    _      -> parens d

palt :: (a -> Doc ann) -> (Int, [a], (Expr a, Doc ann)) -> Doc ann
palt ppr (t,xs,(_,d)) = angles (pretty t) <+> hsep (ppr <$> xs) <+> "->" <+> d

pprCore :: Name -> Doc ann
pprCore = pretty

pprCoreExpr :: CoreExpr -> Doc ann
pprCoreExpr = pprExpr pprCore

{- |
>>> vsep $ pprCoreExpr . sample <$> [1 .. 8]
x
7
Pack{2,2}
f (g x)
f g x
let
    x = 1;
    xs = Pack{1,0}
in
    Pack{2,2} x xs
case f (g x) of
    <1>  -> 0;
    <2> h hs -> succ (length hs)
λ f g x → case f (g x) of
    <1>  -> 0;
    <2> h hs -> succ (length hs)
-}
sample :: Int -> CoreExpr
sample i = fromMaybe (ENum 404) (lookup i sampleExprs)

sampleExprs :: [(Int, CoreExpr)]
sampleExprs
    = 
    [(1, EVar "x")
    ,(2, ENum 7)
    ,(3, EConstr 2 2)
    ,(4, EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
    ,(5, EAp (EAp (EVar "f") (EVar "g")) (EVar "x"))
    ,(6, ELet [("x",ENum 1),("xs", EConstr 1 0)] (EAp (EAp (EConstr 2 2) (EVar "x")) (EVar "xs")))
    ,(7, ECase (sample 4) [(1,[],ENum 0),(2,["h","hs"],EAp (EVar "succ") (EAp (EVar "length") (EVar "hs")))])
    ,(8, ELam ["f","g","x"] (sample 7))
    ]

