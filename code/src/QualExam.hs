{-# LANGUAGE RankNTypes, GADTs, TypeFamilies
           , DataKinds, PolyKinds, ScopedTypeVariables
           , TypeOperators
           , UndecidableInstances, FlexibleInstances
           , MultiParamTypeClasses, FunctionalDependencies
           , FlexibleContexts, TypeFamilyDependencies
#-}

module QualExam where
import GHC.Float
import Data.Proxy
import GHC.Types

data TT
data FF

data Z
data S n
{-
type family TEq a b
type instance TEq a a = TT
type instance TEq a b = FF -- Error!
-}

type family TEq a b where
  TEq a a = TT
  TEq a b = FF
  
type family Or a b where
  Or TT _ = TT
  Or _ TT = TT
  Or _ _  = FF

type family And a b where
  And TT TT = TT
  And TT  a  = a

type family Wacky a b where
  Wacky a a = Char
  Wacky a b = Bool

bad ::  a -> Wacky a (Maybe a)
bad _ = undefined

-- class Add m n where
--   add :: m -> n -> Result m n

-- instance Add Int Int where
--   add = (+)

-- instance Add Int Float where
--   add = (+)


data Leaf a
data Node a b

type family TMember e set where
  TMember e (Leaf e') = TEq e e'
  TMember e (Node lt rt) = Or (TMember e lt) (TMember e rt)

t :: Proxy (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)))
t = Proxy

pt :: Proxy (TMember 1 (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))))
pt = Proxy

pf :: Proxy (TMember 5 (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))))
pf = Proxy

type family CountArgs ty where
  CountArgs (a -> b) = S (CountArgs b)
  CountArgs b        = Z

cpf :: Proxy (CountArgs (Int -> Bool -> (Int -> Bool) -> Char))
cpf = Proxy

type family Loop a where
  Loop Bool = [Bool]
  Loop t = [Loop t]

-- k :: (TEq [Loop] Loop ~ TT) => Bool
-- k = True

type family Elem c
class Collects c where
  empty :: c
  insert :: Elem c -> c -> c
  member :: Elem c -> c -> Bool

type instance Elem [Char] = Char
instance Collects [Char] where
  empty = []
  insert e c = e : c
  member e [] = False
  member e (h:t) = if e == h then True else member e t

type family Result m n where
  Result Int Int   = Int
  Result a   Float = Float
  Result Float a   = Float
  Result a   Int   = a
  Result Int a     = a

class Add m n where
  add :: m -> n -> Result m n

instance Add Int Int where
  add = (+)

instance Add Int Float where
  add i f = (int2Float i) + f

-- instance Add Int Float where
--   type Result Int Float = Int -- Error
--   add = (+)


--- Constraint Type Families

-- loop :: Loop
-- loop = loop

loopy = loopy

class PTyFamC a where
  type PTyFam a :: Type

instance PTyFamC Int where
  type PTyFam Int = Bool

pty :: PTyFam Bool
pty = let loop = loop in loop

type family NoEqFam a where
  {- No Equations-}


pty2 :: NoEqFam Int
pty2 = undefined

-- type family ListElems a = b | b -> a
-- type instance ListElems [a] = a
class Top
instance Top

class Top => IdTyFamC (a::Type) where
  type IdTyFam a :: Type

instance IdTyFamC a where
  type IdTyFam a = a

sillyFst x = fst (x, loopy)

sillyFst' x = fst (x, pty)

class PlusC (m :: *) (n :: *) where
  type Plus m n :: *

instance PlusC Z (n :: *) where
  type Plus Z n = n

instance PlusC m n => PlusC (S m) (n :: *) where
  type Plus (S m) n = S (Plus m n)


type family Trick a where
  Trick FF = Int -> Int
  Trick a  = Bool

funTrick :: a -> Trick (TEq a a)
funTrick _ = id




-- type family TEq a b where
--   TEq a a = TT
--   TEq a b = FF  -- Ok

-- class TEqC a b where
--   type TEq a b

-- instance TEqC a a where
--   type TEq a a = TT

-- instance TEqC a b where
--   type TEq a b = FF


