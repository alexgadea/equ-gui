{-# LANGUAGE TypeSynonymInstances #-}
module Equ.PreExpr.Internal where

import Equ.Syntax
import Control.Applicative ((<$>), (<*>),Applicative(..))
import Test.QuickCheck(Arbitrary, arbitrary, oneof)

data PreExpr' a = Var a
                | Con Constant
                | Fun Func
                | PrExHole Hole
                | UnOp Operator (PreExpr' a)
                | BinOp Operator (PreExpr' a) (PreExpr' a)
                | App (PreExpr' a) (PreExpr' a)
                | Quant Quantifier a (PreExpr' a) (PreExpr' a)
                | Paren (PreExpr' a)
                  deriving Eq


type PreExpr = PreExpr' Variable

instance Functor PreExpr' where
    fmap f (Var a) = Var $ f a
    fmap _ (Con c) = Con c
    fmap _ (Fun g) = Fun g
    fmap _ (PrExHole h) = PrExHole h
    fmap f (UnOp op e) = UnOp op $ fmap f e
    fmap f (BinOp op e e') = BinOp op (fmap f e) (fmap f e')
    fmap f (App e e') = App (fmap f e) (fmap f e')
    fmap f (Quant q a e e') = Quant q (f a) (fmap f e) (fmap f e')
    fmap f (Paren e) = Paren $ fmap f e

-- | Pretty print para las preExpresiones.
instance Show PreExpr where
    show (Var x) = show x
    show (Con k) = show k
    show (Fun f) = show f
    show (PrExHole h) = show h
    show (UnOp op preExp) = show op ++ "(" ++ show preExp ++ ")"
    show (BinOp op preExp0 preExp1) = "(" ++ show preExp0 ++ ")" ++ show op ++ 
                                      "(" ++ show preExp1 ++ ")"
    show (App preExp0 preExp1) = show preExp0 ++ " " ++ "(" ++ show preExp1 ++ ")"
    show (Quant qua v preExp0 preExp1) = show qua ++ show v ++ " in " 
                                        ++ show preExp0 ++ " | " 
                                        ++ show preExp1
    show (Paren e) = "〔" ++ show e ++ " 〕"
                
-- | Instancia arbitrary para las preExpresiones, lo único que dejamos fijo es el 
-- operador unario, esto para simplificar la forma de las preExpresiones.
instance Arbitrary PreExpr where
    arbitrary =
        oneof [   Var <$> arbitrary
                , Con <$> arbitrary
                , Fun <$> arbitrary
                , PrExHole <$> arbitrary
                , UnOp <$> arbitrary <*> arbitrary
                , BinOp <$> arbitrary <*> arbitrary <*> arbitrary
                , App <$> arbitrary <*> arbitrary
                , Quant <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                , Paren <$> arbitrary
                ]


-- Substitucion de variable por variable en preExpresiones.
-- PRE = { v' variable fresca para pe }
substitution :: Eq a => a -> a -> PreExpr' a -> PreExpr' a
substitution v v' e = substVar v v' <$> e
    where substVar w w' w'' | w==w'' = w'
                            | otherwise = w''

{-
-- Substitucion de variable por variable en preExpresiones.
-- A diferencia de substitution, no tenemos la precondicion
-- sobre que v' sea una variable fresca.
-- Sustituye v por v' en pe.
substitution2 :: Variable -> Variable -> PreExpr -> PreExpr
substitution2 v v' (Var v'') = Var $ substVar v v' v''
substitution2 v v' (UnOp op pe) = UnOp op $ substitution2 v v' pe
substitution2 v v' (BinOp op pe pe') = 
    BinOp op (substitution2 v v' pe) (substitution2 v v' pe')
substitution2 v v' (App pe pe') = App (substitution2 v v' pe) (substitution2 v v' pe')
substitution2 v v' (Quant q v'' pe pe') =
    let v_new = searchFreshVar v'' pe pe' -- Variable nueva.
        pe_new = substitution2 v'' v_new pe -- Substitucion de la nueva variable.
        pe_new' = substitution2 v'' v_new pe'-- Substitucion de la nueva variable.
    in Quant q v_new 
            (substitution2 v v' pe_new) 
            (substitution2 v v' pe_new')
substitution2 v v' (Paren pe) = Paren (substitution2 v v' pe)
substitution2 _ _ pe = pe

-- Busca una variable fresca para usar. Asume que la variable
-- que se le pasa por argumento no es fresca.
searchFreshVar :: Variable -> PreExpr -> PreExpr -> Variable
-- Quedo para acomodar a gusto el sufijo de la variable nueva.
searchFreshVar = searchFreshVar' "_new"

-- Auxiliar para calcular searchFreshVar
searchFreshVar' :: String -> Variable -> PreExpr -> PreExpr -> Variable
searchFreshVar' sufix v pe pe' = 
    let name = (T.unpack . tRepr) v ++ sufix
        -- Acá conservo el tipo de v 
        -- Parece no ser necesario, la instancia de Eq
        -- para Variable usa no mas el representante.
        w = var name (varTy v)
    in if   w `L.notElem` subsfreeVars pe &&
            w `L.notElem` subsfreeVars pe'
        then w
        else searchFreshVar' (sufix++sufix) v pe pe'

-- Auxiliar para calcular las variables libres de una preExpresion.
subsfreeVars' :: [Variable] -> PreExpr -> [Variable]
subsfreeVars' lv (Var v) = v : lv
subsfreeVars' lv (Con _) = lv
subsfreeVars' lv (Fun _) = lv
subsfreeVars' lv (PrExHole _) = lv
subsfreeVars' lv (UnOp _ pe) = subsfreeVars' lv pe
subsfreeVars' lv (BinOp _ pe pe') = subsfreeVars' lv pe ++ subsfreeVars' lv pe'
subsfreeVars' lv (App pe pe') = subsfreeVars' lv pe ++ subsfreeVars' lv pe'
subsfreeVars' lv (Paren pe) = subsfreeVars' lv pe
subsfreeVars' lv (Quant _ v pe pe') = L.delete v $ subsfreeVars' lv pe ++ 
                                                   subsfreeVars' lv pe'

-- Dada una preExpresion devuelve una lista con las variables libres.
subsfreeVars :: PreExpr -> [Variable]
subsfreeVars = subsfreeVars' []
    -}