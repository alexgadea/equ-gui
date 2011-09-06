module Equ.Rewrite
    ( exprRewrite
    , focusedRewrite
    , typedRewrite
    , RewriteError
    )
    where

import Equ.Matching
import Equ.Rule
import Equ.Expr
import Equ.PreExpr
import Equ.TypeChecker

-- Constructor del tipo mas general para manejar errores de matching o 
-- unificación de manera indistinta. Como referencia al error usamos el
-- prefijo acerca sobre de que tipo fue el error.
data RewriteError' a b = Matching a | Type b

-- | Tipo general de errores de re-escritura. Contiene errores de matching
-- y unificación.
type RewriteError = RewriteError' (MatchMErr, Log) TyErr

{- | Dada una expresi贸n y una regla, si la expresi贸n matchea con el lado
izquierdo de la regla, entonces se reescribe de acuerdo al lado derecho
de la regla.
-}
exprRewrite :: Expr -> Rule -> Either RewriteError Expr
exprRewrite (Expr e) (Rule{lhs=Expr l,rhs=Expr r}) = 
                            case match l e of
                                Left er -> Left $ Matching er
                                Right subs -> Right $ Expr $ applySubst r subs

-- | Igual a exprRewrite pero ademas retorna la lista de sustituciones.
rewriteInformative :: Expr -> Rule -> Either RewriteError (Expr, ExprSubst)
rewriteInformative (Expr e) (Rule{lhs=Expr l,rhs=Expr r}) = 
                        case match l e of
                            Left er -> Left $ Matching er
                            Right subs -> Right (Expr $ applySubst r subs, subs)

-- | Dado un focus y una regla, aplicamos re-escrituda con la regla a la 
--  expresión focalizada, en caso de exito reemplazamos la expresión inicial
--  por la expresión resultante dentro del focus.
focusedRewrite :: Focus -> Rule -> Either RewriteError Focus
focusedRewrite f@(pe, p) r = exprRewrite (Expr pe) r >>= 
                             \(Expr pe')-> return $ replace f pe'

{- 
    Me di cuenta que no termino de entender que debería hacer esta función.
    Por ejemplo, si hacemos checkPreExpr (parser "0+0") obetenemos
    Right (TyAtom ATyNat) y hasta acá todo bien, hacemos lo mismo para el
    lado izq de la regla. Ahora, con esta substitución que obtengo debería
    cambiar el tipo de 0+0, pero la preExpresion 0+0 no tiene ningun tipo
    asociado fijo que pueda cambiar.
    Sera cambiar el tipo "final", siguiendo con el ejemplo anterior
    tType + = TyAtom ATyNat :-> (TyAtom ATyNat :-> TyAtom ATyNat)
    la idea es cambiar el tipo y que quede;
    tType + = TyAtom ATyNat :-> (TyAtom ATyNat :-> TIPO_NUEVO)
    Me acabo de dar cuenta que solamente se aplicaría a variables de tipo,
    pero bueno aun así me parece que el ejemplo puede ayudar a entender sobre
    lo que dudo.
    
    Dejo escrita la función (con casos incompletos) para ejemplificar lo que
    entiendo. No la termino porque muy probablemente este mal :P
    
    Resolción:
    La idea es re-escribir expresiones tipadas, primero testeando que los tipos
    de estas se puedan unificar, si es así entonces procedemos a re-escribir
    de otra forma devolvemos error de unificación. Aprovechamos que unify 
    tiene un bonito log sobre errores para devolver eso en caso de que no
    existe unificación.
-}
typedRewrite :: Expr -> Rule -> Either RewriteError Expr
typedRewrite e@(Expr pe) ru@(Rule{lhs=Expr l,rhs=Expr r}) = 
    let (Right te) = checkPreExpr pe
        (Right tr) = checkPreExpr l
    in case unify te tr emptySubst of
            Left er -> Left $ Type er
            Right _ -> exprRewrite e ru
