{-# Language OverloadedStrings,DoAndIfThenElse #-}
-- | Modulo de muestra y control de eventos sobre pruebas.
module GUI.Proof where

import GUI.Types hiding (GState)
import GUI.State
import GUI.State.Expr
import GUI.State.Exercise(showChoicesButton)
import GUI.Utils
import GUI.Proof.RelationList

import GUI.Settings
import GUI.TruthList
import Equ.Rule
import Equ.Theories
import Equ.Proof
import qualified Equ.Proof.Proof as P(getStart,getEnd,getBasic,getRel,getCtx)
import Equ.Proof.ListedProof
import Equ.Proof.Annot
import Equ.PreExpr hiding (goDownL,goDownR,goRight,goUp,goTop)
import qualified Equ.PreExpr.Show as PS
import GUI.Widget
import GUI.Expr ( writeExprWidget, writeInitExprWidget
                    , setupForm
                    , newExprState, reloadExpr
                    , createExprWidget
                    , setupOptionExprWidget 
                    )
import GUI.State.SymbolList(eventsSymbolList)
import Equ.Parser hiding (getExprState,getProofState,proof)
import Equ.Types

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import qualified Graphics.UI.Gtk as G
import Graphics.UI.Gtk.Gdk.EventM
--import Graphics.UI.Gtk.Glade (GladeXML,xmlGetWidget)
import Graphics.UI.Gtk.Display.Image

import Data.Maybe(fromJust)
import Data.Text(unpack,pack)
import Data.Map(empty)
import Data.List(elemIndex)

import Control.Monad.Reader
import Control.Applicative((<$>))
import qualified Data.Foldable as F (forM_,mapM_)

import System.Glib.GObject(toGObject)
import System.Random

-- | Crea una nueva referencia
newProofState :: (Maybe Proof) -> HBox -> ExprWidget -> ExprWidget ->
                 ProofStepWidget -> IState ProofState
newProofState (Just p) axiom_box expr1W expr2W proofW= return pr

    where
        pr :: ProofState
        pr = ProofState { proof = fromJust $ createListedProof (toProofFocus p)
                        , validProof = validateProof p
                        , proofWidget = fromJust $ createListedProof (pw,Top)
                        , proofAnnots = fromJust . createListedProof $ emptyProofAnnots
                        }
                        
        pw = Simple () () expr1W expr2W proofW
            
            
newProofState Nothing axiom_box expr1W expr2W proofW = getGlobalCtx >>=
                                  return . pr . Just
    where
        pr :: Maybe Ctx -> ProofState
        pr c = ProofState { proof = fromJust $ createListedProof $ p c
                          , validProof = validateProof $ toProof (p c)
                          , proofWidget = fromJust $ createListedProof (pw,Top)
                          , proofAnnots = fromJust . createListedProof $ emptyProofAnnots
                        }
        p c = emptyProof c $ head $ relationList
        
        pw = Simple () () expr1W expr2W proofW

                        
-- | Carga una prueba a la interfaz. 
loadProof :: Proof -> VBox -> VBox -> ExprWidget -> ProofStepWidget -> IState ()
loadProof p ret_box truthBox initExprWidget proofStepW = do
    
    --newExpr_w  <- newExprWidget (toExpr $ fromRight $ getEnd p) (ProofMove moveToEnd) truthBox
    newExpr_w  <- newExprWidget (toExpr $ fromRight $ getEnd p) 0
    
    -- Expresión inicial:
    removeAllChildren (formBox initExprWidget)
    initExpr <- return . fromRight $ getStart p
    
    initExprWidget' <- flip runEnvBox (initExprWidget,id,0) 
                            (writeInitExprWidget (toExpr initExpr) >>= 
                            \wes -> return (initExprWidget {wExprL = wes}))
    
    empty_box1 <- io $ hBoxNew False 2
    (l_proof,_) <- return $ toHoleProof (p,Top)
    proof <- newProofState (Just l_proof) empty_box1 initExprWidget' newExpr_w proofStepW
    updateProofState proof
    
    pstate <- getProof
    
    unsetExprState
    
    io (boxPackStart ret_box truthBox PackNatural 2 >>
        boxPackStart ret_box (extBox newExpr_w) PackNatural 2)
    
    completeProof p truthBox 0    
    return ()
    
completeProof :: Proof -> VBox -> Int -> IState Int
completeProof p@(Trans _ rel f1 fm f2 p1 p2) center_box ind = do
    (boxL,boxR) <- newStepProof (toExpr fm) ind center_box
    
    ind' <- completeProof p1 boxL ind
    completeProof p2 boxR (ind'+1)

completeProof (Hole _ rel f1 f2) center_box ind =
    --addStepProof center_box top_box moveFocus Nothing >>
    return ind

completeProof p@(Simple _ rel f1 f2 b) center_box ind =
    -- addStepProof center_box top_box moveFocus (Just b) >>
    changeProofFocusAndShow ind >>
    getProofWidget >>= \lpw ->
    writeTruth b (axiomWidget $ fromJust $ getSelBasic lpw) >>
    return ind


-- | Crea toda la estructura necesaria para una nueva prueba.  Si el
-- primer argumento es @Nothing@, entonces se crea la estructura para
-- una prueba vacía; si es @Just p@, entonces se crea para la prueba @p@.
createNewProof :: (Maybe Proof) -> VBox -> VBox -> ExprWidget -> IState ()
createNewProof proof ret_box truthBox initExprWidget = do
    io $ debug "creando prueba..."
    
    -- delete all children
    removeAllChildren ret_box
    
    removeAllChildren (formBox initExprWidget)
    initExpr <- getExpr
    
    initExprWidget' <- flip runEnvBox (initExprWidget,id,0) 
                            (writeInitExprWidget (toExpr initExpr) >>= 
                            \wes -> return (initExprWidget {wExprL = wes}))
    
    -- truthBox es la caja central para colocar la relacion y el axioma aplicado. La
    -- funcion para mover el foco es ir hasta el tope.
    io $ debug $ "Antes de crear el widget de paso de prueba"
    
    firstStepProof <- addStepProof truthBox 0 Nothing
    
    io $ debug $ "Pude crear el widget de paso de prueba"
        
    maybe (emptyProof truthBox initExprWidget' firstStepProof) 
          (\p -> loadProof p ret_box truthBox initExprWidget' firstStepProof >>
                 getProof >>= \pf -> io (debug $ "prueba despues de loadProof:"++show pf)) proof
    
    s <- get    
    io $ widgetShowAll ret_box
    
    showSelectedStep

    where emptyProof box initExprW firstStepW = do
            --hboxInit <- createExprWidget holePreExpr goTopbox
            expr_w  <- newExprWidget holePreExpr 0

            liftIO $ debug "Inicializando estado de prueba..."
            initState initExprW expr_w firstStepW
            
            io (--boxPackStart ret_box hboxInit PackNatural 2 >>
                boxPackStart ret_box box PackNatural 2 >>
                boxPackStart ret_box (extBox expr_w) PackNatural 2
               )


initState :: ExprWidget -> ExprWidget -> ProofStepWidget -> IRG
initState expr1W expr2W proofW = do
    -- TODO: Ver si está bien poner cajas vacias para la caja de expresion y la de 
    -- axiomas al iniciar la referencia. Si el usuario elige un símbolo para construir 
    -- expresión o elige un axioma ANTES de hacer click en alguna caja, entonces la prueba
    -- se actualiza en el estado pero no se muestra en la interfaz (ya que esas cajas vacías
    -- nunca fueron agregadas a la interfaz)
    
    -- inicialmente ponemos una caja vacia en el foco, asumiendo que no hay ninguna
    -- expresión enfocada.
    empty_box1 <- io $ hBoxNew False 2
    initExpr <- getExpr
    proof' <- newProofState (Just $ pr initExpr) empty_box1 expr1W expr2W proofW
    updateProofState proof'
    unsetExprState

    where pr e= flip newProofWithStart e $ head $ relationList
    
checkProof :: Image -> VBox -> IState ()
checkProof validImage top_box = getProofState >>= (F.mapM_ $ \ps ->
                                updateValidProof >> checkValidProof >>= \valid ->
                                if valid 
                                then updateImageValid iconValidProof
                                else getValidProof >>= \(Left errorProof) ->
                                        io (putStrLn (show errorProof)) >>
                                        updateImageValid iconErrorProof)
                                       

-- | Creación de línea de justificación de paso en una prueba.
addStepProof :: VBox -> Int -> Maybe Basic -> IState ProofStepWidget
addStepProof center_box stepIndex maybe_basic = do
    -- top_box es la caja central mas general, que es creada al iniciar una prueba.    
    removeAllChildren center_box
    
    rel <- getRelPF
    hbox <- io $ hBoxNew False 2
    
    -- Relation combo Box
    (combo_rel,store_rel)   <- io $ newComboRel rel
    
    -- Axiom box
    axiom_box  <- io $ hBoxNew False 2
    label      <- io $ labelNew (Just $ emptyLabel "")
    io (widgetSetSizeRequest axiom_box 450 (-1) >>
        boxPackStart axiom_box label PackGrow 0)

    button_box <- io $ hButtonBoxNew    
    addStepProofButton <- io $ makeButtonWithImage addStepIcon
    io $ buttonSetRelief addStepProofButton ReliefNone
    
    io $ setToolTip addStepProofButton "Agregar Paso"
    io $ widgetSetSizeRequest button_box 150 (-1)
    
    eb_axiom_box <- io $ eventBoxNew 
    
    imageValidStep <- io $ imageNewFromStock iconUnknownProof IconSizeSmallToolbar

    io (widgetSetSizeRequest combo_rel 80 (-1) >>
            boxPackStart button_box addStepProofButton PackNatural 2 >>
            boxPackStart hbox combo_rel PackNatural 5 >>
            set eb_axiom_box [ containerChild := axiom_box ] >>
            boxPackStart hbox eb_axiom_box PackGrow 5 >> 
            boxPackStart hbox button_box PackNatural 1 >>
            boxPackStart hbox imageValidStep PackNatural 1 >>
            highlightBox hbox axiomBg)
        
    io $ boxPackStart center_box hbox PackNatural 5
    
    flip F.mapM_ maybe_basic $ flip writeTruth axiom_box
    
    -- ran <- io $ randomIO
    
    psw <- return ProofStepWidget {
                    relation = (combo_rel,store_rel) 
                  , axiomWidget = axiom_box 
                  , addStepButton = addStepProofButton
                  , eventBoxAxiom = eb_axiom_box
                  , validImage = imageValidStep
                  , stepBox = hbox
                  , centerBox = center_box
                  , stepEventsIds = []
                  , stepProofIndex = stepIndex
                  , pswId = show 0 -- show (mod (ran :: Int) 200)
    }
    
    psw' <- eventsProofStep psw
    
    return psw'                 
                                       
newStepProof :: PreExpr -> Int -> VBox -> IState (VBox,VBox)
newStepProof expr stepIndex container = do
    removeAllChildren container

    -- Movemos el ProofFocus hasta donde está el hueco que queremos reemplazar
    -- por una transitividad
    
    
    changeProofFocusAndShow stepIndex
        -- Reemplazamos el hueco por una transitividad
    
    
    lp <- getProof
    lpw <- getProofWidget
    lpa <- getProofAnnots

    updateProofUndo (addStepOnPosition stepIndex (fProof (toFocus expr)) (\e i -> e) (\p i -> p) lp) 
    
    relation <- getRelPF
    
    centerBoxL <- io $vBoxNew False 2
    newStepWL <- addStepProof centerBoxL stepIndex Nothing
    centerBoxR <- io $ vBoxNew False 2
    newStepWR <- addStepProof centerBoxR (stepIndex + 1) Nothing
    expr_w <- newExprWidget expr stepIndex
    
    io (boxPackStart container centerBoxL PackNatural 5 >>
            boxPackStart container (extBox expr_w) PackNatural 5 >>
            boxPackStart container centerBoxR PackNatural 5 >>
        
            widgetShowAll container)
    
    liftIO $ debug $ "ProofWidget antes de agregar: " ++ show lpw
    
    lpw' <- return $ addStepOnPosition stepIndex (fProofWidget expr_w newStepWL newStepWR)
                                    fUpIndexExprW fUpIndexProofW lpw

    lpa' <- return $ addStepOnPosition stepIndex fProofAnnot const const lpa

    liftIO $ debug $ "ProofWidget despues de agregar: " ++ show lpw'
                                    
    lpw'' <- runActionLP lpw' (stepIndex+1) resetSignalsStep
    -- Nota: queda enfocado el paso stepIndex+1
    
    updateProofWidget lpw''
    updateProofAnnots lpa'
    
    showSelectedStep

    return (centerBoxL,centerBoxR)
    
    {- Cuando se modifique la expresion que queda en el medio de esta transitividad,
       tenemos que actualizar la expr del medio de la transitividad, la expr final de la
       prueba izquierda y la expr inicial de la prueba derecha. Para hacer todo esto vamos moviéndonos
       con el zipper
       -}
       where 
            fProof :: Focus -> Proof -> (Focus,Proof,Proof)
            fProof expr proof = let (ctx,rel,f1,f2) = (fromJust $ P.getCtx proof,
                                               fromJust $ P.getRel proof,
                                               fromJust $ P.getStart proof,
                                               fromJust $ P.getEnd proof) in
                                (expr,(Hole ctx rel f1 expr),
                                    (Hole ctx rel expr f2))
                
            fProofWidget :: ExprWidget -> ProofStepWidget -> ProofStepWidget ->
                            ProofWidget -> (ExprWidget,ProofWidget,ProofWidget)
            fProofWidget expr_w newStepWL newStepWR p = 
                  (expr_w,Simple () () (fromJust $ P.getStart p) expr_w newStepWL,
                            Simple () () expr_w (fromJust $ P.getEnd p) newStepWR)

            fProofAnnot p = ( annot
                            , Simple () () (fromJust . P.getStart $ p) annot ()
                            , Simple () () annot (fromJust . P.getEnd $ p) ())
                where annot = pack ""
                            
            fUpIndexExprW :: ExprWidget -> Int -> ExprWidget
            fUpIndexExprW expr_w ind = expr_w { exprProofIndex = ind }
            
            fUpIndexProofW :: ProofStepWidget -> Int -> ProofStepWidget
            fUpIndexProofW step_w ind = step_w { stepProofIndex = ind }
            
            
            
newExprWidget :: PreExpr -> Int -> IState ExprWidget              
newExprWidget expr stepIndex = do
    
    exprWidget <- createExprWidget False stepIndex
    exprWidget' <- flip runEnvBox (exprWidget,id,stepIndex) 
                        (writeExprWidget expr >>= 
                        \wes -> return (exprWidget {wExprL = wes}))
    exprWidget'' <- eventsExprWidget exprWidget'
    return exprWidget''

    
-- | Setea los eventos de un widget de expresion. La funcion f es la
-- que se utiliza para actualizar la expresion dentro de la prueba
eventsExprWidget :: ExprWidget -> IState ExprWidget
eventsExprWidget exprWidget = let stepIndex = exprProofIndex exprWidget in
    do
    io (debug $ "Indice: " ++ show stepIndex)
    io $ debug $ "Seteando eventos para eWidget :"++ show exprWidget ++" con indice "++ show stepIndex
    s <- get
    win <- getWindow
    cids <- io (setupFocusEvent s stepIndex)
    (cid3,cid4) <-  flip runEnvBox (exprWidget,id,stepIndex) (setupOptionExprWidget win)
    return $ exprWidget {exprEventsIds = cids++[Connectable cid3,Connectable cid4]}
    
    where 
        hb = extBox exprWidget
        setupFocusEvent :: GRef -> Int -> IO [Connectable]
        setupFocusEvent s stepIndex = do
            cid1 <- hb `on` buttonReleaseEvent $ do
                    flip eventWithState s $
                    -- movemos el proofFocus hasta donde está esta expresión.
                            io (debug $ "Expresión clickeada con indice: " ++ show stepIndex) >>
                            --updateExprWidget exprWidget  >>
                            changeProofFocus' stepIndex
                    io (widgetShowAll hb)
                    return True

            listRw <- evalStateT showChoicesButton s
            if listRw 
            then do 
                let Just choices = choicesButton exprWidget
            
                cid2 <- choices `on` buttonPressEvent $ tryEvent $
                            eventWithState (changeProofFocus' stepIndex >> showChoices stepIndex) s
                return [Connectable cid1,Connectable cid2]
            else return [Connectable cid1]

        changeProofFocus' stepIndex = changeProofFocusAndShow stepIndex 
                                    -- >> updateSelectedExpr -- Actualizamos la expresion seleccionada
                    
        showChoices stepIndex = do
            menu <- io menuNew
            pf <- getProof
            exp1 <- return $ getStartExpr pf
            m_axiom <- return $ getSelBasic pf
            flip F.mapM_ m_axiom $ \axiom -> 
                return (possibleExpr (toExpr exp1) axiom) >>=
                addToMenu menu stepIndex >>
                io (widgetShowAll menu >> menuPopup menu Nothing)
        
        addToMenu m stepIndex = mapM_ addItem
            where 
                addItem (e, mf) = do
                    item <- io $ menuItemNewWithLabel $ PS.showExpr e
                    io $ menuShellAppend m item
                    s' <- get
                    io $ item `on` buttonPressEvent $ tryEvent $
                        flip eventWithState s' $ 
                            -- Actualizamos la expresion
                            changeProofFocus' stepIndex >>
                            updateExprWidget exprWidget >>
                            runEnvBox (writeExprWidget e) 
                                      (exprWidget, id,stepIndex) >>
                            updateExpr e id
          
-- | Funcion para resetear los manejadores de eventos de expresiones y pasos de prueba.
-- Solo se realiza con las expresiones derechas. Las expresiones izquierdas se encuentran
-- siempre a la derecha de algun otro paso y se resetean en ese momento.
resetSignalsStep :: ProofWidget -> IState (ExprWidget,ProofStepWidget)
resetSignalsStep pw = case pw of
                           (Simple _ _ _ e b) -> do
                               liftIO $ debug $ "-- ResetSignals " ++ show pw ++ " --"
                               let ls1 = exprEventsIds e
                               let ls2 = stepEventsIds b
                               io $ mapM_ signalDisconnect' ls1
                               io $ mapM_ signalDisconnect' ls2
                               e' <- eventsExprWidget e
                               b' <- eventsProofStep b
                               return (e',b')
                               
    where signalDisconnect' (Connectable cid) = signalDisconnect cid
          
                                  
eventsProofStep :: ProofStepWidget -> IState ProofStepWidget
eventsProofStep psw = do
    let ind = stepProofIndex psw
    s <- get
    combo_rel <- return (fst $ relation psw)
    store_rel <- return (snd $ relation psw)
    eb_axiom_box <- return (eventBoxAxiom psw)
    axiom_box <- return (axiomWidget psw)
    
    cid1 <- addHandler eb_axiom_box buttonPressEvent (do
        LeftButton <- eventButton
        io $ debug "axiom_box clicked"
        eventWithState (changeProofFocusAndShow ind) s)
        
        
    cid2 <- addHandler eb_axiom_box  buttonPressEvent (do
        RightButton <- eventButton
        io $ debug "axiom_box right clicked"
        eventWithState (changeProofFocusAndShow ind >>
                        removeAllChildren axiom_box) s

        label <- io (labelNew (Just $ emptyLabel ""))
        io $ boxPackStart axiom_box label PackGrow 0
        eventWithState (getProof >>= updateProofUndo . resetStep) s
        io $ widgetShowAll axiom_box)
        
    cid3 <- io $ (addStepButton psw) `on` buttonPressEvent $ 
       flip eventWithState s (newStepProof holePreExpr ind (centerBox psw)) >>
       return False
       
    cid4 <- io (combo_rel `on` changed $ evalStateT (changeItem combo_rel store_rel ind) s)
       
    return psw { stepEventsIds = [Connectable cid1,Connectable cid2,Connectable cid3,
                                  Connectable cid4] }
       
    where changeItem c list ind= do 
            changeProofFocusAndShow ind
            ind <- io $ comboBoxGetActive c
            newRel <- io $ listStoreGetValue list ind
            updateRelation newRel
            validateStep

-- | Descarta la prueba actual.
discardProof :: ContainerClass c => c -> ExprWidget -> IState ()
discardProof centralBox expr_w = unsetProofState >>
                                 removeAllChildren centralBox >>
                                 getExpr >>= \e ->
                                 io (debug $ "discardingProof, expresion = "++ show e) >>
                                 io (hBoxNew False 2) >>=
                                 newExprState e expr_w >>=
                                 updateExprState >>
                                 runEnvBox (reloadExpr (toExpr e)) (expr_w,id,0) >>
                                 return ()

                    
                                      
showSelectedStep = selectBox focusBg                 
