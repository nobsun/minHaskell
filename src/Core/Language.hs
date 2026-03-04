-- # Core.Language
-- コア言語の構文
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
module Core.Language
    where

import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable

type Name        = String

data Expr a
    = EVar Name
    | ENum Int
    | EConstr Tag Arity
    | EAp (Expr a) (Expr a)
    | ELet [Binder a] (Expr a)
    | ECase (Expr a) [Alter a]
    | ELam [a] (Expr a)
    deriving (Eq, Show, Read)

type Tag   = Int
type Arity = Int

type Binder a = (a, Expr a)
type Alter a  = (Tag, [a], Expr a)

type CoreProgram = Program Name
type Program a   = [ScDefn a]
type CoreScDefn  = ScDefn Name
type ScDefn a    = (Name, [a], Expr a)
type CoreExpr    = Expr Name

data ExprF a r
    = EVarF Name
    | ENumF Int
    | EConstrF Tag Arity
    | EApF r r
    | ELetF [BinderF a r] r
    | ECaseF r [AlterF a r ]
    | ELamF [a] r
    deriving (Eq, Show, Read, Functor)

type BinderF a r = (a, r)
type AlterF a r  = (Tag, [a], r)

type instance Base (Expr a) = ExprF a

instance Recursive (Expr a) where
    project :: Expr a -> ExprF a (Expr a)
    project = \ case
        EVar v       -> EVarF v
        ENum n       -> ENumF n
        EConstr t a  -> EConstrF t a
        EAp e1 e2    -> EApF e1 e2
        ELet bs e    -> ELetF bs e
        ECase e alts -> ECaseF e alts
        ELam vs e    -> ELamF vs e

instance Corecursive (Expr a) where
    embed :: ExprF a (Expr a) -> Expr a
    embed = \ case
        EVarF v       -> EVar v
        ENumF n       -> ENum n
        EConstrF t a  -> EConstr t a
        EApF e1 e2    -> EAp e1 e2
        ELetF bs e    -> ELet bs e
        ECaseF e alts -> ECase e alts
        ELamF xs e    -> ELam xs e

cataExpr :: (ExprF a b -> b) -> Expr a -> b
cataExpr = cata

paraExpr :: (ExprF a (Expr a, b) -> b) -> Expr a -> b
paraExpr = para

zygoExpr :: (ExprF a c -> c) -> (ExprF a (c, b) -> b) -> Expr a -> b
zygoExpr = zygo

anaExpr :: (b -> ExprF a b) -> b -> Expr a
anaExpr = ana

apoExpr :: (b -> ExprF a (Either (Expr a) b)) -> b -> Expr a
apoExpr = apo

hyloExpr :: (ExprF a c -> c) -> (b -> ExprF a b) -> b -> c
hyloExpr = hylo

{- AnnProgram -}

type AnnProgram a ann = [AnnScDefn a ann]
type AnnScDefn a ann = (Name, [a], AnnExpr a ann)
type AnnExpr a = Cofree (ExprF a)
type AnnBinders a ann = [(a, AnnExpr a ann)]
type AnnAlter a ann = (Tag, [a], AnnExpr a ann)
type AnnAlters a ann = [AnnAlter a ann]

cataAnnExpr :: (F.CofreeF (ExprF a) ann b -> b)
            -> AnnExpr a ann -> b
cataAnnExpr = cata

paraAnnExpr :: (F.CofreeF (ExprF a) ann (AnnExpr a ann, b) -> b)
            -> AnnExpr a ann -> b
paraAnnExpr = para

zygoAnnExpr :: (F.CofreeF (ExprF a) ann c -> c)
            -> (F.CofreeF (ExprF a) ann (c, b) -> b)
            -> AnnExpr a ann -> b
zygoAnnExpr = zygo

anaAnnExpr :: (b -> F.CofreeF (ExprF a) ann b)
           -> b -> AnnExpr a ann
anaAnnExpr = ana

apoAnnExpr :: (b -> F.CofreeF (ExprF a) ann (Either (AnnExpr a ann) b))
           -> b -> AnnExpr a ann
apoAnnExpr = apo

hyloAnnExpr :: (F.CofreeF (ExprF a) ann c -> c)
            -> (b -> F.CofreeF (ExprF a) ann b)
            -> b -> c
hyloAnnExpr = hylo

deAnnProg :: AnnProgram a ann -> Program a
deAnnProg = map deAnnScDefn

deAnnScDefn :: AnnScDefn a ann -> ScDefn a
deAnnScDefn = \ case
    (name, as, ae) -> (name, as, deAnnExpr ae)

deAnnExpr :: AnnExpr a ann -> Expr a
deAnnExpr = cataAnnExpr phi where
    phi = \ case
        _ F.:< EVarF v       -> EVar v
        _ F.:< ENumF n       -> ENum n
        _ F.:< EConstrF t a  -> EConstr t a
        _ F.:< EApF e1 e2    -> EAp e1 e2
        _ F.:< ELetF bs e    -> ELet bs e
        _ F.:< ECaseF e alts -> ECase e alts
        _ F.:< ELamF xs e    -> ELam xs e
