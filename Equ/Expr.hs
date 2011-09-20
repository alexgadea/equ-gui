-- | Una Expresión es algo que se puede manipular. Difiere
-- relativamente poco de una PreExpresión.
module Equ.Expr where

import Equ.PreExpr.Internal
-- import Equ.Theories
-- import Equ.Syntax

-- | Las expresiones son pre-expresiones bien tipadas. Es decir,
-- ningún constituyente de una expresión puede tener TyUnknown como
-- tipo.
newtype Expr = Expr PreExpr

instance Show Expr where 
    show (Expr e) = show e

instance Eq Expr where
    (==) (Expr e1) (Expr e2) = e1==e2
    
getPreExpr :: Expr -> PreExpr
getPreExpr (Expr e) = e
