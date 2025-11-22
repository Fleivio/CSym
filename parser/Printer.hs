{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Printer(pretty, innerStr) where

import Data.Complex
import Data.Number.CReal

import Ast

tabSize :: Int -> String
tabSize k = take (k*2) $ cycle ['|', ' ']

data PString = PString {innerStr :: String} deriving (Eq, Show)

instance Semigroup PString where
  PString a <> PString b = PString $ a ++ "\n" ++ b

instance Monoid PString where
  mempty = PString ""

class Pretty a where
  pretty :: Int -> a -> PString

instance {-# OVERLAPPABLE #-} Show a => Pretty a where
  pretty sp val = PString (tabSize sp ++ show val)

instance Pretty a => Pretty (Complex a) where
  pretty sp cpl = PString (tabSize sp ++ "Complex:")
                  <> pretty (succ sp) (realPart cpl) 
                  <> PString (innerStr (pretty (succ sp) (imagPart cpl)) ++ "*i")

instance Pretty CReal where
  pretty sp cr = PString (tabSize sp ++ showCReal 4 cr)

instance Pretty Type where
  pretty sp Ty_U = PString (tabSize sp ++ "Ty_Unit")
  pretty sp (Ty_Sum t1 t2) = PString (tabSize sp ++ "Ty_Sum:") 
                                <> pretty (succ sp) t1 
                                <> pretty (succ sp) t2
  pretty sp (Ty_Prod t1 t2) = PString (tabSize sp ++ "Ty_Prod:") 
                                 <> pretty (succ sp) t1 
                                 <> pretty (succ sp) t2
  pretty sp (Ty_List t) = PString (tabSize sp ++ "Ty_List:") 
                            <> pretty (succ sp) t
  pretty sp (Ty_Var v) = PString (tabSize sp ++ "Ty_Var: " ++ v)

instance Pretty IsoType where
  pretty sp (IT_Iso t1 t2) = PString (tabSize sp ++ "IT_Iso:") 
                                <> pretty (succ sp) t1 
                                <> pretty (succ sp) t2
  pretty sp (IT_Func it1 it2) = PString (tabSize sp ++ "IT_Func:") 
                                  <> pretty (succ sp) it1 
                                  <> pretty (succ sp) it2

instance Pretty Value where
  pretty sp V_Unit = PString (tabSize sp ++ "V_Unit")
  pretty sp (V_Var v) = PString (tabSize sp ++ "V_Var: " ++ v)
  pretty sp (V_Inl v) = PString (tabSize sp ++ "V_Inl:") 
                          <> pretty (succ sp) v
  pretty sp (V_Inr v) = PString (tabSize sp ++ "V_Inr:") 
                          <> pretty (succ sp) v
  pretty sp (V_Pair v1 v2) = PString (tabSize sp ++ "V_Pair:") 
                             <> pretty (succ sp) v1 
                             <> pretty (succ sp) v2
  pretty sp (V_List_Cons v1 v2) = PString (tabSize sp ++ "V_List_Cons:") 
                                   <> pretty (succ sp) v1 
                                   <> pretty (succ sp) v2
  pretty sp V_List_Empty = PString (tabSize sp ++ "V_List_Empty")

instance Pretty CombValue where
  pretty sp (CV_Value v) = PString (tabSize sp ++ "CV_Value:") 
                            <> pretty (succ sp) v
  pretty sp (CV_Add cv1 cv2) = PString (tabSize sp ++ "CV_Add:") 
                               <> pretty (succ sp) cv1 
                               <> pretty (succ sp) cv2
  pretty sp (CV_Mult s cv) = PString (tabSize sp ++ "CV_Mult:") 
                             <> pretty (succ sp) s 
                             <> pretty (succ sp) cv

instance Pretty Product where
  pretty sp P_Unit = PString (tabSize sp ++ "P_Unit")
  pretty sp (P_Var v) = PString (tabSize sp ++ "P_Var: " ++ v)
  pretty sp (P_Pair p1 p2) = PString (tabSize sp ++ "P_Pair:") 
                             <> pretty (succ sp) p1 
                             <> pretty (succ sp) p2

instance Pretty Extended where
  pretty sp (E_Value cv) = PString (tabSize sp ++ "E_Value:") 
                           <> pretty (succ sp) cv
  pretty sp (E_Assign p1 iso p2 ext) = PString (tabSize sp ++ "E_Assign:") 
                                       <> pretty (succ sp) p1 
                                       <> pretty (succ sp) iso 
                                       <> pretty (succ sp) p2
                                       <> pretty (succ sp) ext

instance Pretty Iso where
  pretty sp (I_Set lst) = PString (tabSize sp ++ "I_Set:") 
                           <> mconcat [ pretty (succ sp) v 
                                        <> pretty (succ sp) ext 
                                      | (v, ext) <- lst ]
  pretty sp (I_Lam name iso) = PString (tabSize sp ++ "I_Lam: " ++ name) 
                                 <> pretty (succ sp) iso
  pretty sp (I_Mu name iso) = PString (tabSize sp ++ "I_Mu: " ++ name) 
                                <> pretty (succ sp) iso
  pretty sp (I_Name name) = PString (tabSize sp ++ "I_Name: " ++ name)
  pretty sp (I_App iso1 iso2) = PString (tabSize sp ++ "I_App:") 
                                 <> pretty (succ sp) iso1 
                                 <> pretty (succ sp) iso2
  pretty sp (I_Adj iso) = PString (tabSize sp ++ "I_Adj:") 
                           <> pretty (succ sp) iso

instance Pretty Term where
  pretty sp T_Unit = PString (tabSize sp ++ "T_Unit")
  pretty sp (T_Var v) = PString (tabSize sp ++ "T_Var: " ++ v)
  pretty sp (T_Inl t) = PString (tabSize sp ++ "T_Inl:") 
                         <> pretty (succ sp) t
  pretty sp (T_Inr t) = PString (tabSize sp ++ "T_Inr:") 
                         <> pretty (succ sp) t
  pretty sp (T_Sum t1 t2) = PString (tabSize sp ++ "T_Sum:") 
                              <> pretty (succ sp) t1 
                              <> pretty (succ sp) t2
  pretty sp (T_App iso t) = PString (tabSize sp ++ "T_App:") 
                            <> pretty (succ sp) iso 
                            <> pretty (succ sp) t
  pretty sp (T_Ext p t1 t2) = PString (tabSize sp ++ "T_Ext:") 
                               <> pretty (succ sp) p 
                               <> pretty (succ sp) t1 
                               <> pretty (succ sp) t2
  pretty sp (T_List_Cons t1 t2) = PString (tabSize sp ++ "T_List_Cons:") 
                                   <> pretty (succ sp) t1 
                                   <> pretty (succ sp) t2
  pretty sp (T_List ts) = PString (tabSize sp ++ "T_List:") 
                          <> mconcat (map (pretty (succ sp)) ts)
  pretty sp (T_Add t1 t2) = PString (tabSize sp ++ "T_Add:") 
                            <> pretty (succ sp) t1 
                            <> pretty (succ sp) t2
  pretty sp (T_Mult s t) = PString (tabSize sp ++ "T_Mult:") 
                           <> pretty (succ sp) s 
                           <> pretty (succ sp) t

instance Pretty Def where
  pretty sp (Def_Iso name it iso) = PString (tabSize sp ++ "Def_Iso: " ++ name) 
                                     <> pretty (succ sp) it 
                                     <> pretty (succ sp) iso
  pretty sp (Def_Term name ty term) = PString (tabSize sp ++ "Def_Term: " ++ name) 
                                       <> pretty (succ sp) ty 
                                       <> pretty (succ sp) term
  pretty sp (Def_Type name ty) = PString (tabSize sp ++ "Def_Type: " ++ name) 
                                    <> pretty (succ sp) ty