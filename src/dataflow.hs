module Dataflow where

import Control.Monad (liftM)
import Control.Monad.Fix (MonadFix(..), fix) 
import Data.Functor.Identity (Identity)

mmap :: Monad t => (a -> b) -> t a -> t b
mmap = liftM

mstrength :: Monad t => t a -> b -> t (a, b)
mstrength c b = c >>= \a -> return (a, b)

newtype Id a = Id a
             deriving (Show)

instance Functor Id where
  fmap f (Id a) = Id $ f a
  
instance Applicative Id where
  pure = Id
  Id f <*> Id a = Id $ f a
  
instance Monad Id where
  return a = Id a
  Id a >>= k = k a
  
-- -- maybe is already in prelude
-- -- list too
-- -- I think this is the (->) monad or Reader?
-- newtype Exp e a = Exp (e -> a)

-- instance Functor (Exp e) where
--   fmap f (Exp g) = Exp (f . g)

-- instance Applicative (Exp e) where
--   pure a = Exp (\_ -> a)
--   -- (e -> a -> b) -> (e -> a) -> (e -> b)
--   (Exp f) <*> (Exp k) = Exp (\e -> f e (k e))
  
-- instance (Applicative (Exp e)) => Monad (Exp e) where
--   return = pure
--   -- (e -> a) -> (a -> (e -> b)) -> (e -> b)
--   Exp f >>= k = Exp (\e -> case k (f e) of
--                        Exp f' -> f' e)

-- state is in prelude

raise :: Maybe a
raise = Nothing

handle :: Maybe a -> Maybe a -> Maybe a
Just a `handle` _ = Just a
Nothing `handle` c = c

deadlock :: [a]
deadlock = []

choice :: [a] -> [a] -> [a]
choice as0 as1 = as0 ++ as1

-- askE :: Exp e e
-- askE = Exp id

-- -- localE :: (e -> e) -> Exp e a -> Exp e b in paper??
-- localE :: (e -> e) -> Exp e a -> Exp e a
-- localE g (Exp f) = Exp (f . g)

type Var = String

data Tm = V Var | L Var Tm | Tm :@ Tm -- variables, lambdas, application
        | N Integer | Tm :+ Tm | Tm :- Tm | Tm :* Tm       -- numbers and addition and stuff
        | Tm :== Tm | Tm :<= Tm | TT | FF | Not Tm | If Tm Tm Tm -- equality and bool stuff
        | Error | Tm `Handle` Tm -- specific for Maybe
        | Deadlock | Tm `Choice` Tm -- specific for []
        | Rec Tm -- recursion
        deriving (Show)
                 
-- value types will be numbers, bools and functions
data Val t = I Integer | B Bool | F (Val t -> t (Val t))
                    
type Env t = [(Var, Val t)]

empty :: [(a, b)]
empty = []

update :: a -> b -> [(a, b)] -> [(a, b)]
update x y xys = (x, y) : xys

unsafeLookup :: Eq a => a -> [(a, b)] -> b
unsafeLookup a0 ((a, b):abs) = if a0 == a then b else unsafeLookup a0 abs

class MonadFix t => MonadEv t where
  ev :: Tm -> Env t -> t (Val t)

instance MonadFix Id where
  mfix k = fix (k . unId)
    where
      unId (Id a) = a
      
evClosed :: MonadEv t => Tm -> t (Val t)
evClosed e = ev e empty

-- V Var | L Var Tm | Tm :@ Tm -- variables, lambdas, application
--         | N Integer | Tm :+ Tm        -- numbers and addition
--         | Tm :== Tm | TT | FF | Not Tm | If Tm Tm Tm -- equality and
--                                          -- bool stuff
--         | Error | Tm `Handle` Tm -- specific for Maybe
--         | Deadlock | Tm `Choice` Tm
--         | Rec Tm
-- data Val t = I Integer | B Bool | F (Val t -> t (Val t))

_ev :: MonadEv t => Tm -> Env t -> t (Val t)
_ev (V x) env = return $ unsafeLookup x env
-- TODO dont really understand
_ev (L x e) env = return $ F (\a -> ev e (update x a env))
_ev (e :@ e') env = do
  (F k) <- ev e env
  a <- ev e' env
  k a

_ev (N n) _env = return (I n)  
_ev (e0 :+ e1) env = do
  (I n0) <- ev e0 env
  (I n1) <- ev e1 env
  return $ I (n0 + n1)
_ev (e0 :- e1) env = do
  (I n0) <- ev e0 env
  (I n1) <- ev e1 env
  return $ I (n0 - n1)
_ev (e0 :* e1) env = do
  (I n0) <- ev e0 env
  (I n1) <- ev e1 env
  return $ I (n0 * n1)
  
_ev (e0 :== e1) env = do
  (I n0) <- ev e0 env
  (I n1) <- ev e1 env
  return $ B (n0 == n1)
_ev (e0 :<= e1) env = do
  (I n0) <- ev e0 env
  (I n1) <- ev e1 env
  return $ B (n0 <= n1)
_ev TT _env = return (B True)
_ev FF _env = return (B False)
_ev (Not e) env = do
  (B x) <- ev e env
  return (B (not x))
_ev (If e e0 e1) env = do
  (B b) <- ev e env
  if b then ev e0 env else ev e1 env

_ev (Rec e) env = do
  (F k) <- ev e env
  mfix k
  
instance MonadEv Id where
  ev e env = _ev e env

instance MonadEv Maybe where
  ev Error _env = raise
  ev (e0 `Handle` e1) env = ev e0 env `handle` ev e1 env
  ev e env = _ev e env

instance MonadEv [] where
  ev Deadlock _env = deadlock
  ev (e0 `Choice` e1) env = ev e0 env `choice` ev e1 env
  ev e env = _ev e env
  

fact :: Tm
fact = Rec (L "fact" (L "x" (
                          If (V "x" :<= N 1)
                          (N 1)
                          ((V "fact" :@ (V "x" :- N 1)) :* V "x"))))
