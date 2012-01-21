-- | Este módulo define el tipo de las reglas de re-escritura.

module Equ.Rule (
                  Relation (..)
                , Rule (..)
                -- * Constructores de las diferentes relaciones.
                , relEq, relEquiv, relImpl, relCons
                )
    where
    
import Equ.Expr
import Equ.Types

import Data.Text    
import Data.Serialize

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck(Arbitrary, arbitrary, elements)

-- | Nombres de las relaciones entre pasos de una demostracion
data RelName = Eq     -- ^ Igualdad polimorfica excepto para formulas
             | Equiv  -- ^ FOL: equivalencia
             | Impl   -- ^ FOL: implicacion
             | Cons   -- ^ FOL: consecuencia
    deriving (Eq, Show, Enum)

instance Arbitrary RelName where
    arbitrary = elements [ Eq
                         , Equiv
                         , Impl
                         , Cons
                         ]

-- | Relaciones entre pasos de una demostracion
data Relation = Relation {
      relRepr :: Text
    , relName :: RelName
    , relTy   :: Type -- ^ Este es el tipo de las cosas relacionadas.  
    }
    deriving Eq
    
instance Show Relation where
    show (Relation _ n _) = show n

instance Arbitrary Relation where
    arbitrary = Relation <$> arbitrary <*> arbitrary <*> arbitrary

-- | Relación de igualdad ; =
relEq :: Relation
relEq = Relation { relRepr = pack "="
                 , relName = Eq
                 , relTy = tyVar "A"
                 }

-- | Relación de equivalencia ; ≡
relEquiv :: Relation
relEquiv = Relation { relRepr = pack "≡"
                    , relName = Equiv
                    , relTy = tyBool
                    }

-- | Relación de implicación ; "⇒"
relImpl :: Relation
relImpl = Relation { relRepr = pack "⇒"
                   , relName = Impl
                   , relTy = tyBool
                   }

-- | relación de consecuencia ; ⇐
relCons :: Relation
relCons = Relation { relRepr = pack "⇐"
                   , relName = Cons
                   , relTy = tyBool
                   }
    
-- | Regla de reescritura
data Rule = Rule {
      lhs :: Expr
    , rhs :: Expr
    , rel :: Relation  
    , name :: Text
    , desc :: Text
    }
    deriving (Show, Eq)

instance Arbitrary Rule where
    arbitrary = Rule <$> arbitrary <*> arbitrary <*> 
                         arbitrary <*> arbitrary <*> arbitrary

instance Serialize Rule where
    put (Rule lhs rhs rel n desc) = put lhs >> put rhs >> put rel >>
                                    put n >> put desc  

    get = Rule <$> get <*> get <*> get <*> get <*> get

instance Serialize RelName where
    put = putWord8 . toEnum . fromEnum
    get = getWord8 >>= return . toEnum . fromEnum

instance Serialize Relation where
    put (Relation r n t) = put r >> put n >> put t
    get = Relation <$> get <*> get <*> get


