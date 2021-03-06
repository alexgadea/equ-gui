{-# Language OverloadedStrings #-}
-- | Utilidades varias que tienen que ver con el estado de la
-- interfaz (es probable que se muden a GUI.State) y con
-- funciones convenientes que podrían mudarse a otros módulos.
module GUI.Utils where

import GUI.Types

import Equ.Expr
import Equ.PreExpr
import Equ.Theories
import Equ.Syntax
import Equ.Parser

import Equ.Proof.Proof
import Equ.Proof.Error(errEmptyProof)
import Equ.Proof(ProofFocus,updateStartFocus,updateEndFocus,PM,validateProof,toProof)
import Equ.Rule

import Equ.Types

import Graphics.UI.Gtk hiding (eventButton, eventSent, get)
import qualified Graphics.UI.Gtk as G
import System.Glib.GType
import System.Glib.GObject

import Data.Text (unpack)
import Data.List
import Data.Either(rights)

import Data.Reference
import Control.Arrow(first,second,(***),(&&&))
import Control.Monad(liftM)
import Control.Monad.State(get,put,evalStateT)
import Control.Monad.Trans(MonadIO,liftIO)


import qualified Data.Serialize as S
import qualified Data.ByteString as L
import qualified Data.Foldable as F (mapM_) 

-- | 
withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust = flip F.mapM_

-- | Composición bastante usada; podría ir a Equ.PreExpr.Internal.
repr :: Syntactic t => t -> String
repr = unpack . tRepr

{- Las tres funciones que siguen podrían ir a Equ.PreExpr.Zipper -}
-- | Una función insegura; sólo la usamos cuando sabemos que la usamos
-- bien.
go :: (Focus -> Maybe Focus) -> Move
go g e = maybe (error $ show e) id $ g e

-- | Composición de ida-vuelta.
(.^.) :: GoBack -> GoBack -> GoBack
(f,f') .^. (g,g') = (f . g , g' . f')

-- | Composición (insegura) de ida-vueltas.
(.^) :: (Focus -> Maybe Focus,Focus -> Maybe Focus) -> GoBack -> GoBack
(f,f') .^ (g,g') = (go f . g , g' . go f')


-- | Pone un mensaje en una área de estado.
putMsg :: StatusPlace -> String -> IO ()
putMsg st m = uncurry statusbarPush st m >> return ()
                 

-- DONDE VAN ESTAS FUNCIONES???
encodeFile :: S.Serialize a => FilePath -> a -> IO ()
encodeFile f v = L.writeFile f (S.encode v)
 
decodeFile :: S.Serialize a => FilePath -> IO a
decodeFile f = L.readFile f >>= 
               either error return .
                      S.runGet (S.get >>= \v ->
                                S.isEmpty >>= \m ->
                                m `seq` return v)
                                                         
setFileFilter :: FileChooserClass f => f -> [String] -> String -> IO ()
setFileFilter fChooser patterns title = do
    hsfilt <- fileFilterNew
    mapM_ (fileFilterAddPattern hsfilt) patterns
    fileFilterSetName hsfilt title
    fileChooserAddFilter fChooser hsfilt



isVBox :: WidgetClass w => w -> Bool
isVBox w = isA w gTypeVBox 

isHBox :: WidgetClass w => w -> Bool
isHBox w = isA w gTypeHBox 

fromRight = head . rights . return          

-- | Funcion para emitir mensajes de debugging.
debug :: String -> IO ()
debug = const $ return ()
-- debug = putStrLn

io :: MonadIO m => IO a -> m a
io = liftIO

iconValidProof :: StockId
iconValidProof = stockOk

iconErrorProof :: StockId
iconErrorProof = stockCancel

iconUnknownProof :: StockId
iconUnknownProof = stockDialogQuestion

ctw :: GObjectClass obj => obj -> Widget
ctw = castToWidget
