{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Ast(
    Scalar
  , Type(..)
  , IsoType(..)
  , Value(..)
  , CombValue(..)
  , Product(..)
  , Extended(..)
  , Iso(..)
  , Term(..)
  , Def(..)
  , Defs
) where

import Data.Complex
import Data.Number.CReal

type Scalar = Complex CReal

data Type
  = Ty_U
  | Ty_Sum Type Type
  | Ty_Prod Type Type
  | Ty_List Type
  | Ty_Var String
  deriving(Eq, Show)

data IsoType
  = IT_Iso Type Type
  | IT_Func IsoType IsoType -- LOOKUP: Original was (IsoType <-> IsoType) -> IsoType
  deriving(Eq, Show)

data Value
  = V_Unit
  | V_Var String
  | V_Inl Value
  | V_Inr Value
  | V_Pair Value Value
  | V_List_Cons Value Value
  | V_List_Empty 
  deriving(Eq, Show)

data CombValue
  = CV_Value Value
  | CV_Add CombValue CombValue
  | CV_Mult Scalar CombValue
  deriving(Eq, Show)

data Product
  = P_Unit
  | P_Var String
  | P_Pair Product Product
  deriving(Eq, Show)

data Extended
  = E_Value CombValue
  | E_Assign Product Iso Product Extended
  deriving(Eq, Show)

data Iso
  = I_Set [(Value, Extended)]
  | I_Lam String Iso
  | I_Mu String Iso
  | I_Name String
  | I_App Iso Iso
  | I_Adj Iso
  deriving(Eq, Show)

data Term
  = T_Unit
  | T_Var String
  | T_Inl Term
  | T_Inr Term
  | T_Sum Term Term
  | T_App Iso Term
  | T_Ext Product Term Term
  | T_List_Cons Term Term
  | T_List [Term]
  | T_Add Term Term
  | T_Mult Scalar Term
  deriving(Eq, Show)

-- Top-level definitions
data Def
  = Def_Iso String IsoType Iso
  | Def_Term String Type Term
  | Def_Type String Type
    deriving(Eq, Show)

type Defs = [Def]