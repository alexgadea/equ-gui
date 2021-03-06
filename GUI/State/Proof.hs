{-# Language OverloadedStrings #-}
-- | Utilidades varias que tienen que ver con el estado de la
-- interfaz (es probable que se muden a GUI.State) y con
-- funciones convenientes que podrían mudarse a otros módulos.
module GUI.State.Proof
    where

import GUI.Types
import GUI.State.Internal hiding (local)
import GUI.Utils

import Equ.Rule (Relation, relEq)

import Equ.Proof.Proof
import Equ.Proof.Error(errEmptyProof)
import Equ.Proof(ProofFocus,ProofFocus',updateStartFocus,updateEndFocus,PM,validateProof,
                 toProof,goFirstLeft,updateMiddleFocus,goUp',getEndFocus,goRight,goEnd,goDownL',
                  getBasicFocus, validateListedProof,validateStepProof, goNextStep, goPrevStep)
import Equ.Proof.ListedProof
import Equ.Proof.Annot

import Graphics.UI.Gtk (HBox,StockId,imageSetFromStock,IconSize(IconSizeSmallToolbar), castToHBox, containerForeach, Color, widgetModifyBg,WidgetClass)
import GUI.Settings
import Data.Maybe (fromJust,isJust)
import Data.List (find)
import qualified Data.Foldable as F

import Control.Arrow(first,(&&&))
import Control.Monad.Trans(liftIO)

getProofState :: IState (Maybe ProofState)
getProofState = getStatePartDbg "getProofState" gProof


getProof :: IState ListedProof
getProof = getStatePartDbg "getProof" (proof . fromJust . gProof)

getProofWidget :: IState ListedProofWidget
getProofWidget = getStatePartDbg "getProofWidget" (proofWidget . fromJust . gProof)

safeGetProofWidget :: IState (Maybe ListedProofWidget)
safeGetProofWidget = 
        askRef >>= \s ->
        case isJust $ gProof s of
        True -> getStatePartDbg "getProofWidget" 
                (proofWidget . fromJust . gProof) >>= return . Just 
        False -> return Nothing

getValidProof :: IState (PM Proof)
getValidProof = getStatePart (maybe (Left errEmptyProof) validProof . gProof)

updateImageValid :: StockId -> IRG
updateImageValid icon = getStatePart imageValid >>= \validImage ->
                        io (imageSetFromStock validImage icon IconSizeSmallToolbar)

restoreValidProofImage :: IRG
restoreValidProofImage = updateImageValid iconUnknownProof

-- Las siguientes funciones validan el paso en el que la prueba está enfocada.
validateStep :: (Proof -> IState ()) -> IState ()
validateStep accion = getProofState >>= 
               F.mapM_ (\ps -> getProof >>= \lp ->
               case validateStepProof lp of
                    Left er -> updateStepWidgetImage iconErrorProof
                    Right p -> accion p >> updateStepWidgetImage iconValidProof
                    )

updateStepWidgetImage :: StockId -> IState ()
updateStepWidgetImage icon = getProofState >>= 
                        F.mapM_ (\ps -> getProofWidget >>= \pfw ->
                        let image = validImage (fromJust $ getSelBasic pfw) in
                             io (imageSetFromStock image icon IconSizeSmallToolbar)
                        )

updateProof' :: ListedProof -> GState -> GState
updateProof' lp gst = case (gProof gst,gExpr gst) of
                           (Just gpr,_) -> upd gpr
                           (_,_) -> gst
    where upd gpr = gst { gProof = Just gpr { proof = lp}
                        }

-- | Valida la prueba y actualiza el campo "validProof" del ProofState
updateValidProof :: IState ()
updateValidProof = getValidProof >>= \vp -> update (updateValidProof' vp)
    where updateValidProof' :: PM Proof -> GState -> GState
          updateValidProof' vp gst = case gProof gst of
                                       Just gpr -> gst { gProof = Just $ updPrf gpr }
                                       Nothing -> gst
          updPrf gpr = gpr { validProof = validateListedProof (proof gpr) }

updateProofWidget pfw = update (\gst -> case gProof gst of
                                             Nothing -> gst
                                             Just gpr -> gst {gProof = Just gpr {
                                                     proofWidget = pfw}
                                             })

showProof :: IState ()
showProof = (withRefValue $ uncurry putMsg . (status &&& show . proof . fromJust . gProof ) ) >>
            io (debug "showProof") >> showProof'

showProof' = getProof >>= io . debug . show

addTheorem :: Theorem -> IState Theorem
addTheorem th = (update $ \gst -> gst { theorems = (th:theorems gst) }) >>
                return th

getTheorems :: IState [Theorem]
getTheorems = getStatePart theorems

getIndexBasic:: ExprWidget -> IState (Maybe Int)
getIndexBasic ew = safeGetProofWidget >>= 
                   maybe (return Nothing) (return . Just . getIndexBasic' ew 0)
    where
        getIndexBasic' :: ExprWidget -> Int -> ListedProofWidget -> Int
        getIndexBasic' ew i lpw | getBasicAt i lpw == ew = i
                                | otherwise = getIndexBasic' ew (i+1) lpw

getRelPF :: IState Relation
getRelPF = getProofState >>= \ps ->
            case ps of
                 Nothing -> return relEq
                 Just ps' -> 
                    getStatePart $ getRelLP . proof . fromJust . gProof

updateProofAnnots :: ListedAnnots -> IState ()
updateProofAnnots pfa = update (updateProofAnnots' pfa)

updateProofAnnots' :: ListedAnnots -> GState -> GState
updateProofAnnots' pfa gst = case gProof gst of
                                Nothing -> gst
                                Just gpr -> upd gpr
    where
        upd :: ProofState -> GState
        upd gpr = gst {gProof = Just gpr {proofAnnots = pfa}}


getProofAnnots :: IState ListedAnnots
getProofAnnots = getStatePartDbg "getProof" (proofAnnots . fromJust . gProof)
