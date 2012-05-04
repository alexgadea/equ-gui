module GUI.Proof.Dialogs where

import GUI.Types
import GUI.State
import GUI.Utils
import GUI.Settings
import GUI.Proof 
import GUI.Widget
import Equ.Proof
import Equ.Proof.ListedProof
import Equ.Parser.Proof
import Graphics.UI.Gtk hiding (eventButton, eventSent,get)

import Data.Maybe(isJust)
import Control.Monad(when)
import qualified Data.Foldable as F (mapM_)

import qualified System.IO.Strict as SIO
import System.FilePath (takeExtension)

dialogLoadProof :: GRef -> VBox -> VBox -> ExprWidget -> IO ()
dialogLoadProof ref centralBox truthBox expr_w = do
    dialog <- fileChooserDialogNew (Just "Cargar Prueba") 
                                  Nothing 
                                  FileChooserActionOpen
                                  [ ("Cargar",ResponseAccept)
                                  , ("Cancelar",ResponseCancel)]

    equFileFilter dialog 
    response <- io $ dialogRun dialog
    
    case response of
         ResponseAccept -> do
             selected <- io $ fileChooserGetFilename dialog
             io $ debug ("aceptar clicked. Selected is " ++ show selected)
             flip F.mapM_ selected (\ filepath -> 
                                        (case takeExtension filepath of 
                                          ".prf" -> loadTextualPrf filepath
                                          ".equ" -> loadBinaryPrf filepath)
                                        (\prf -> evalStateT (createNewProof (Just prf) centralBox truthBox expr_w) ref >>
                                         widgetDestroy dialog))
         _ -> io $ widgetDestroy dialog

loadTextualPrf filepath action = SIO.readFile filepath >>= \cts ->
                 either (putStrLn . show) action (parsePfFromString' cts)
                 
loadBinaryPrf filepath action = decodeFile filepath >>= action

saveProofDialog :: IRG
saveProofDialog = do
    pf <- getProofState
    when (isJust pf) $ do 
      dialog <- io $ fileChooserDialogNew (Just "Guardar Prueba") 
                                         Nothing 
                                         FileChooserActionSave 
                                         [ ("Guardar",ResponseAccept)
                                         , ("Cancelar",ResponseCancel)]
                                   
      equFileFilter dialog                                   
      response <- io $ dialogRun dialog
    
      case response of
        ResponseAccept -> io (fileChooserGetFilename dialog) >>= F.mapM_ saveProof
        _ -> return ()
      io $ widgetDestroy dialog
                         
saveProof :: FilePath -> IRG
saveProof filepath = getProof >>= io . encodeFile filepath . listedToProof

equFileFilter dialog = io $ setFileFilter dialog ["*.equ","*.prf"] "Prueba de Equ"

