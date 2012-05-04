-- | Interfaz gráfica de Equ.
module Main where

import GUI.Exercise
import GUI.Types
import GUI.State
import GUI.State.SymbolList
import GUI.State.Expr
import GUI.Utils
import GUI.Widget
import GUI.Settings
import GUI.Expr
import GUI.SymbolList
import GUI.TruthList
import GUI.Proof 
import GUI.TypeTree
import GUI.Truth
import GUI.Undo
import GUI.Proof.Dialogs

import GUI.State.Exercise

import Equ.PreExpr(holePreExpr, emptyExpr)
import Equ.Theories (theories)

import qualified Graphics.UI.Gtk as G
import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.UI.Gtk.Gdk.Events 
import Graphics.UI.Gtk.Glade
import Data.Text (pack,unpack)

import Data.Reference

import Control.Monad(liftM, when)
import Control.Monad.Reader

import qualified Data.Foldable as F (forM_)

import Data.Map (fromList)


main :: IO ()
main = do 
    initGUI

    -- TODO: qué pasa si no existe el archivo.
    Just xml <- xmlNew "GUI/equ.glade"

    -- get widgets
    window        <- xmlGetWidget xml castToWindow "mainWindow"
    quitButton    <- getMenuButton xml "QuitButton"

    statusBar     <- xmlGetWidget xml castToStatusbar "statusBar"
    ctxExpr       <- statusbarGetContextId statusBar "Expr"
    symbolList    <- xmlGetWidget xml castToIconView "symbolList"
    axiomList     <- xmlGetWidget xml castToTreeView "axiomList"
    
    symFrame <- xmlGetWidget xml castToFrame "symFrame"
    axiomFrame <- xmlGetWidget xml castToFrame "axiomFrame"
    errPane <- xmlGetWidget xml castToPaned "errPane"

    centralBox <- xmlGetWidget xml castToVBox "centralBox"


    itemNewProof <- xmlGetWidget xml castToImageMenuItem "itemNewProof"
    itemLoadProof <- xmlGetWidget xml castToImageMenuItem "itemLoadProof"
    itemSaveProof <- xmlGetWidget xml castToImageMenuItem "itemSaveProof"
    itemDiscardProof <- xmlGetWidget xml castToImageMenuItem "itemDiscardProof"
    itemSaveAsTheorem <- xmlGetWidget xml castToImageMenuItem "itemSaveAsTheorem"
    
    itemUndo <- xmlGetWidget xml castToImageMenuItem "undoMenuItem"
    itemRedo <- xmlGetWidget xml castToImageMenuItem "redoMenuItem"
    
    exerciseEditToolBarBox <- xmlGetWidget xml castToToolbar "exerciseEditToolBarBox"
    exerciseToolBarBox <- xmlGetWidget xml castToToolbar "exerciseToolBarBox"
    
    itemMakeExercise <- xmlGetWidget xml castToImageMenuItem "itemMakeExercise"
    itemSaveExercise <- xmlGetWidget xml castToImageMenuItem "itemSaveExercise"
    itemLoadForEditExercise <- xmlGetWidget xml castToImageMenuItem "itemLoadForEditExercise"
    
    itemSaveProofExercise <- xmlGetWidget xml castToImageMenuItem "itemSaveProofExercise"
    itemLoadExercise <- xmlGetWidget xml castToImageMenuItem "itemLoadExercise"
    
    -- toolbuttons
    newProofTool <- xmlGetWidget xml castToToolButton "newProof"
    discardProofTool <- xmlGetWidget xml castToToolButton "discardProof"
    saveTheoremTool <- xmlGetWidget xml castToToolButton "saveTheoremTool"
    saveHypothesisTool <- xmlGetWidget xml castToToolButton "saveHypothesisTool"

    unDo <- xmlGetWidget xml castToToolButton "undoTool"
    reDo <- xmlGetWidget xml castToToolButton "redoTool"
    
    fieldProofFaceBox <- xmlGetWidget xml castToHBox "fieldProofFaceBox"
    
    proofFaceBox <- xmlGetWidget xml castToHBox "proofFaceBox"
            
    -- Expresión inicial
    initExprBox <- xmlGetWidget xml castToHBox "initExprBox"
    
    -- Validar Prueba
    validTool <- xmlGetWidget xml castToToolButton "validateTool"
    itemValidateProof <- xmlGetWidget xml castToImageMenuItem "itemValidateProof"
    boxValidIcon <- xmlGetWidget xml castToHBox "boxValidIcon"
    imageValidProof <- imageNewFromStock iconUnknownProof IconSizeSmallToolbar
    boxPackStart boxValidIcon  imageValidProof PackNatural 2
    
    symGoLeftBox <- xmlGetWidget xml castToHBox "symGoLeftBox"
    symGoRightBox <- xmlGetWidget xml castToHBox "symGoRightBox"
    swSymbolList <- xmlGetWidget xml castToScrolledWindow "swSymbolList"
    
    truthBox <- io $ vBoxNew False 2

    windowMaximize window
    sListStore <- io $ setupSymbolList symbolList 

    gRef <- newRef $ initialState window symbolList sListStore axiomList Nothing statusBar ctxExpr imageValidProof

    -- Agregamos los botones pero sin visibilidad.
    evalStateT (setupExerciseEditToolbar exerciseEditToolBarBox) gRef
    evalStateT (setupExerciseToolbar exerciseToolBarBox) gRef
    
    onActivateLeaf quitButton $ quitAction window
    onDestroy window mainQuit

    setupScrolledWindowSymbolList swSymbolList symGoLeftBox symGoRightBox gRef
    aListStore <- io $ setupTruthList  theories [] axiomList window
        
    -- Define la misma acción para el boton que para el menu
    -- convención: "nombreItem" (para item de menu) y "nombreTool" (para botón)
    getAndSetAction xml "saveHypothesis" (addHypothesisUI aListStore) gRef

    -- expresion inicial
    evalStateT (initExprState emptyExpr) gRef
    initExprWidget <- evalStateT (loadExpr initExprBox holePreExpr id) gRef

    setActionMenuTool itemNewProof newProofTool (createNewProof Nothing centralBox truthBox initExprWidget) gRef    

    onActivateLeaf itemSaveProof $ evalStateT saveProofDialog gRef    
    setActionMenuTool itemDiscardProof discardProofTool (discardProof centralBox initExprWidget) gRef
    setActionMenuTool itemValidateProof validTool (checkProof imageValidProof truthBox) gRef
    
    setActionMenuTool itemUndo unDo (undoEvent centralBox truthBox initExprWidget) gRef
    setActionMenuTool itemRedo reDo (redoEvent centralBox truthBox initExprWidget) gRef
    
    onActivateLeaf itemMakeExercise $ 
                   evalStateT (showAllItemTool exerciseEditToolBarBox >> 
                               makeExercise) gRef 
    onActivateLeaf itemSaveExercise $ evalStateT saveExercise gRef 
    onActivateLeaf itemLoadForEditExercise $ 
                   evalStateT (loadExercise >>= \flag ->
                               when (flag) (setupProofFromExercise
                                                          centralBox 
                                                          truthBox 
                                                          initExprWidget >>
                                            showAllItemTool exerciseEditToolBarBox)
                                ) gRef 
    
    onActivateLeaf itemSaveProofExercise $ 
                   evalStateT (return ()) gRef 
    
    onActivateLeaf itemLoadExercise $ 
                   evalStateT (loadExercise >>= \flag ->
                                when (flag)
                                     (setupProofFromExercise
                                                          centralBox 
                                                          truthBox 
                                                          initExprWidget >>
                                      showAllItemTool exerciseToolBarBox)
                              ) gRef 
    
    onActivateLeaf itemSaveAsTheorem $ saveTheorem gRef aListStore
    onToolButtonClicked saveTheoremTool $ saveTheorem gRef aListStore

    onActivateLeaf itemLoadProof $ dialogLoadProof gRef centralBox truthBox initExprWidget
    
    flip evalStateT gRef $ do
        axioms <- getAxiomCtrl
        eventsTruthList axioms aListStore
        symbols <- getSymCtrl
        runEnvBox (eventsSymbolList symbols sListStore) (initExprWidget,id,0)
        hidePane errPane

    widgetShowAll window

    mainGUI
          
-- | Funcion que define el mismo evento para un menuItem y un toolButton.
setActionMenuTool item tool act ref = onToolButtonClicked tool action >>
                                      onActivateLeaf item action
    where action = evalStateT act ref 

-- TODO: unificar nombres de botones y menú de items de manera que se pueda
-- usar la funcion getAndSetAction
-- | Funcion que dado el nombre de un control que está en el menu y en la barra
-- configura la misma acción para ambos.
getAndSetAction xml name action ref = do
  item <- xmlGetWidget xml castToImageMenuItem $ name ++ "Item"
  tool <- xmlGetWidget xml castToToolButton $ name ++ "Tool"
  setActionMenuTool item tool action ref