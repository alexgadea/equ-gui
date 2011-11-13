module TestSuite.Tests.Parser (testGroupParse) where

import Equ.Parser
import Equ.PreExpr

import TestSuite.Tests.Samples


import Test.HUnit (Assertion, assertFailure)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (unless)

testsParser :: [Test]
testsParser = map (\(str, expr) -> testCase (encloseQuotes str) (testParse str expr))
                [ ("p", Var p)
                , ("ys", Var ys)
                , ("S@0", sApp0)
                , ("S@y", sAppy)
                , ("(x+S@0)", xPlussApp0)
                , ("x + S@y + z", xPlusSyPlusZ)
                , ("S@y + S@(x+S@0) + z", sAppyPlusSomePlusz)
                , ("[x]", listX)
                , ("[y,w]", listYW)
                , ("[x] ++ [y,w]", listXConcatListYW)
                , ("#([x] ++ [y,w])", lengthList)              
                -- TODO: Este es un caso en el que falla el parser. Para mas info del error
                -- es interesante hacer goDown de la expresión esperada contra la expresión
                -- parseada.
                , ("(#([x] ++ [y,w])) + z", lengthListPlusz)
                ]
    where encloseQuotes str = "\""++ str ++"\""

-- Controlamos que el parseo de un string sea el esperado, comparandolo con
-- la preExpresion que le pasamos. TODO: Sobre el control de errores,
-- ParseError no tiene instancia de Eq, esto genera un problema para comparar.
-- Por esa razon de momento no testeamos errores informativos de parseo.
testParse :: String -> PreExpr -> Assertion
testParse s pe = case parseFromString s of
                   Left _ -> assertFailure "Error de parseo."
                   Right pe' -> unless (pe == pe') $
                               assertFailure $ 
                               "\n Resultado esperado: " ++ show pe ++
                               "\n Contra : " ++ show pe'

testGroupParse :: Test
testGroupParse = testGroup "Parser" testsParser
