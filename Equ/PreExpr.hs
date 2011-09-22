-- | Las PreExpresiones son árboles de expresiones no necesariamente
-- tipables con huecos. Como se comenta en el módulo Equ.Syntax, el
-- tipo que posiblemente puso el usuario está en las hojas del árbol.
{-# Language OverloadedStrings #-}
module Equ.PreExpr ( freeVars, freshVar
                   , encode, decode, encodeFile, decodeFile
                   , preExprHole, isPreExprHole
                   , module Equ.Syntax
                   , module Equ.PreExpr.Internal
                   , module Equ.PreExpr.Zipper
                   , module Equ.PreExpr.Monad
                   ) 
    where


import Equ.Syntax(Variable, Operator, Quantifier, var, HoleInfo, hole)
import Data.Set (Set,union,delete,empty,insert,member)
import Equ.Types
import Equ.PreExpr.Internal
import Equ.PreExpr.Zipper
import Equ.PreExpr.Monad

import Data.Text(pack)
import Data.Binary(encode, decode, encodeFile, decodeFile)

import Equ.Parser
import Equ.Theories.AbsName

isPreExprHole :: Focus -> Bool
isPreExprHole (PrExHole _, _) = True
isPreExprHole _ = False

preExprHole :: HoleInfo -> PreExpr
preExprHole i = PrExHole $ hole i

-- | Esta funcion devuelve todas las variables libres de una expresion
freeVars :: PreExpr -> Set Variable
freeVars (Var v) = insert v empty
freeVars (Con _) = empty
freeVars (Fun _) = empty
freeVars (PrExHole _) = empty
freeVars (UnOp _ e) = freeVars e
freeVars (BinOp _ e1 e2) = freeVars e1 `union` freeVars e2
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Quant _ v e1 e2) = delete v $ freeVars e1 `union` freeVars e2
freeVars (Paren e) = freeVars e

-- | Esta funcion devuelve una variable fresca con respecto a un conjunto de variables
freshVar :: Set Variable -> Variable
freshVar s = firstNotIn s infListVar
    where infListVar = [var (pack $ "v" ++ show n) TyUnknown | n <- [(0::Int)..]]
          -- PRE: xs es infinita
          firstNotIn set xs | head xs `member` set = firstNotIn set $ tail xs
                            | otherwise = head xs
