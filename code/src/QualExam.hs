{-# LANGUAGE RankNTypes, GADTs, TypeFamilies
           , DataKinds, PolyKinds, ScopedTypeVariables
           , TypeOperators, GeneralisedNewtypeDeriving
           , UndecidableInstances, FlexibleInstances
           , MultiParamTypeClasses, FunctionalDependencies
           , FlexibleContexts, UndecidableSuperClasses
#-}

module QualExam where
import GHC.Float

data TT
data FF

{-
type family TEq a b
type instance TEq a a = TT
type instance TEq a b = FF -- Error!
-}

type family TEq a b where
  TEq a a = TT
  TEq a b = FF  -- Ok


type family Loop
type instance Loop = [Loop]

-- k :: TEq Loop Loop
-- k = undefined


class Collects c where
  type Elem c
  empty :: c
  insert :: Elem c -> c -> c
  member :: Elem c -> c -> Bool


instance Collects [Char] where
  type Elem [Char] = Char
  empty = []
  insert e c = e : c
  member e [] = False
  member e (h:t) = if e == h then True else member e t


type family F a where
  F Int = Int

f x = fst (x, undefined :: F Bool)

class Add m n where
  type Result m n
  add :: m -> n -> Result m n


instance Add Int Int where
  type Result Int Int = Int
  add = (+)

instance Add Int Float where
  type Result Int Float = Float
  add i f = (int2Float i) + f

-- instance Add Int Float where
--   type Result Int Float = Int -- Error
--   add = (+)

