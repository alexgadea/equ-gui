module GUI.State.Ctx ( getGlobalCtx
                         , getGlobalHypothesis
                         , addGlobalHypothesis
                         ) where

import GUI.Types
import GUI.State.Internal    
import GUI.State.Proof

import GUI.Utils

import Equ.PreExpr hiding(goUp,goRight,goLeft,goDown,goDownL)
import Equ.Proof(addBoolHypothesis)
import Equ.Proof.Proof

-- Funciones para manipular y obtener la lista de hipótesis
getGlobalCtx :: IState Ctx
getGlobalCtx = getStatePart hypothesis

-- | Intenta agregar una hipotesis al contexto global.
addGlobalHypothesis :: PreExpr -> IState (Maybe Name)
addGlobalHypothesis e = askRef >>= addHyp
    where addHyp st = case addBoolHypothesis e (hypothesis st) of
                        (ctx,Nothing) -> return Nothing
                        (ctx',Just n) -> update (\st -> st { hypothesis = ctx' }) >> (return $ Just n)

getGlobalHypothesis :: Name -> IState (Maybe Hypothesis)
getGlobalHypothesis n = getGlobalCtx >>= return . getHypothesis n