{-# LANGUAGE RankNTypes, GADTs, TypeFamilies
           , DataKinds, PolyKinds, ScopedTypeVariables
           , TypeOperators, GeneralisedNewtypeDeriving
           , UndecidableInstances, FlexibleInstances
           , MultiParamTypeClasses, FunctionalDependencies
           , FlexibleContexts, UndecidableSuperClasses
#-}

module QualExam where
import GHC.Float
import Data.Proxy

data TBool = TT | FF
data Z
data S n
{-
type family TEq a b
type instance TEq a a = TT
type instance TEq a b = FF -- Error!
-}

type family TEq (a :: k) (b :: k) :: TBool where
  TEq a a = 'TT
  TEq a b = 'FF  -- Ok

type family Or (a :: TBool) (b :: TBool) where
  Or 'TT _ = 'TT
  Or _ 'TT = 'TT
  Or _ _  = 'FF

type family And a b where
  And 'TT 'TT = 'TT
  And 'TT  a  = a

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

type family Loop
type instance Loop = [Loop]

-- k :: TEq Loop Loop
-- k = undefined

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

