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
-- import Graphics.UI.Gtk.Glade
import Data.Text (pack,unpack)

import Data.Reference

import Control.Monad(liftM, when)
import Control.Monad.Reader

import qualified Data.Foldable as F (forM_)

import Data.Map (fromList)


main :: IO ()
main = do 
    initGUI
    xml <- builderNew
    builderAddFromFile xml "GUI/equ.ui"

    -- get widgets
    window        <- builderGetObject xml castToWindow "mainWindow"
    quitButton    <- getMenuButton xml "QuitButton"

    statusBar     <- builderGetObject xml castToStatusbar "statusBar"
    ctxExpr       <- statusbarGetContextId statusBar "Expr"
    symbolList    <- builderGetObject xml castToIconView "symbolList"
    axiomList     <- builderGetObject xml castToTreeView "axiomList"
    
    symFrame <- builderGetObject xml castToFrame "symFrame"
    axiomFrame <- builderGetObject xml castToFrame "axiomFrame"
    errPane <- builderGetObject xml castToPaned "errPane"

    centralBox <- builderGetObject xml castToVBox "centralBox"


    itemNewProof <- builderGetObject xml castToImageMenuItem "itemNewProof"
    itemLoadProof <- builderGetObject xml castToImageMenuItem "itemLoadProof"
    itemSaveProof <- builderGetObject xml castToImageMenuItem "itemSaveProof"
    itemDiscardProof <- builderGetObject xml castToImageMenuItem "itemDiscardProof"
    itemSaveAsTheorem <- builderGetObject xml castToImageMenuItem "itemSaveAsTheorem"
    
    itemUndo <- builderGetObject xml castToImageMenuItem "undoMenuItem"
    itemRedo <- builderGetObject xml castToImageMenuItem "redoMenuItem"
    
    exerciseEditToolBarBox <- builderGetObject xml castToToolbar "exerciseEditToolBarBox"
    exerciseToolBarBox <- builderGetObject xml castToToolbar "exerciseToolBarBox"
    
    itemMakeExercise <- builderGetObject xml castToImageMenuItem "itemMakeExercise"
    itemSaveExercise <- builderGetObject xml castToImageMenuItem "itemSaveExercise"
    itemLoadForEditExercise <- builderGetObject xml castToImageMenuItem "itemLoadForEditExercise"
    
    itemSaveProofExercise <- builderGetObject xml castToImageMenuItem "itemSaveProofExercise"
    itemLoadExercise <- builderGetObject xml castToImageMenuItem "itemLoadExercise"
    
    -- toolbuttons
    newProofTool <- builderGetObject xml castToToolButton "newProof"
    discardProofTool <- builderGetObject xml castToToolButton "discardProof"
    saveTheoremTool <- builderGetObject xml castToToolButton "saveTheoremTool"
    saveHypothesisTool <- builderGetObject xml castToToolButton "saveHypothesisTool"

    unDo <- builderGetObject xml castToToolButton "undoTool"
    reDo <- builderGetObject xml castToToolButton "redoTool"
    
    fieldProofFaceBox <- builderGetObject xml castToHBox "fieldProofFaceBox"
    
    proofFaceBox <- builderGetObject xml castToHBox "proofFaceBox"
            
    -- Expresión inicial
    initExprBox <- builderGetObject xml castToHBox "initExprBox"
    
    -- Validar Prueba
    validTool <- builderGetObject xml castToToolButton "validateTool"
    itemValidateProof <- builderGetObject xml castToImageMenuItem "itemValidateProof"
    boxValidIcon <- builderGetObject xml castToHBox "boxValidIcon"
    imageValidProof <- imageNewFromStock iconUnknownProof IconSizeSmallToolbar
    boxPackStart boxValidIcon  imageValidProof PackNatural 2
    
    symGoLeftBox <- builderGetObject xml castToHBox "symGoLeftBox"
    symGoRightBox <- builderGetObject xml castToHBox "symGoRightBox"
    swSymbolList <- builderGetObject xml castToScrolledWindow "swSymbolList"
    
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
  item <- builderGetObject xml castToImageMenuItem $ name ++ "Item"
  tool <- builderGetObject xml castToToolButton $ name ++ "Tool"
  setActionMenuTool item tool action ref
