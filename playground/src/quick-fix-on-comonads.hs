{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, UndecidableInstances, DataKinds, KindSignatures, GADTs, TypeFamilies, MultiParamTypeClasses, TypeOperators, TypeInType #-}

-- From YT https://www.youtube.com/watch?v=F7F-BzOB670
-- code at https://github.com/kwf/ComonadSheet

import Prelude hiding (iterate)

data Stream a = Cons a (Stream a)

head :: Stream a -> a
head (Cons x _) = x

tail :: Stream a -> Stream a
tail (Cons _ xs) = xs

iterate :: (a -> a) -> a -> Stream a
iterate f x = Cons x (iterate f $ f x)

-- Kmett: "Tape is actually the CoWriter Comonad in disguise" b/c it's isomorphic to Integer -> a. Each distributive Functor is a Representable which just means it has this isomorphism but you don't necessarily know the domain, just that it exists. Representables are also isomorphic to the reader monad it appears.
data Tape a = Tape (Stream a) a (Stream a)

moveL :: Tape a -> Tape a
moveL (Tape (Cons l ls) c rs) = Tape ls l (Cons c rs)
moveR :: Tape a -> Tape a
moveR (Tape ls c (Cons r rs)) = Tape (Cons c ls) r rs

iterateTape :: (a -> a) -> (a -> a) -> a -> Tape a
iterateTape prev next x = Tape (iterate prev $ prev x) x (iterate next $ next x)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs
              
instance Show a => Show (Stream a) where
  show = showStream 5 where
    showStream :: Int -> Stream a -> String
    showStream n s = show $ take n $ streamToList s


-- MonoLocalbinds was only necessary because I used this type of context, if I use just `Show a` instead it works
-- instance (Show (Stream a)) => Show (Tape a) where
--   show (Tape l _ _ ) = show l  -- showTape 4 where

instance Show a => Show (Tape a) where
  show = showTape 4 where
    showTape :: Int  -> Tape a -> String
    showTape n (Tape l c r) = show $ ls ++ [c] ++ rs
      where ls = reverse $ take n $ streamToList l
            rs = take n $ streamToList r

-- | In a Comonad you can always extract a value and put up another layer of structure around it.
class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)

instance Functor Stream where
  fmap f (Cons x s) = Cons (f x) $ fmap f s
instance Functor Tape where
  fmap f (Tape l c r) = Tape (f <$> l) (f c) (f <$> r)
instance Comonad Tape where
  extract (Tape _ c _) = c
  duplicate = iterateTape moveL moveR

-- | Loeb's Theorem: B(B P -> P) -> B P where B stands for Box
-- | Piponi in his blog post tries to find out what the box could stand for in Haskell and starts with a Functor

-- Each function in the input calculates one element of the output list but having access to the whole list. Avoid dependencies if you want the program to halt.
-- loeb :: Functor f => f (f a -> a) -> f a
-- loeb fs = xs
--   where xs = fmap ($ xs) fs

-- loeb [length] --> [1]
-- loeb [length, length] --> [2, 2]
-- loeb [length, (!! 2), (!! 0) + 1] --> [3, 4, 4]

-- rewrite loeb using fix
fix :: (a -> a) -> a
fix f = let x = f x
        in x
-- any recursive function can be expressed in terms of fix

-- defining it an explicit fix does not really change anything
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = fix $ \xs -> fmap ($ xs) fs


-- Now we try to instantiate Loeb's Theorem with a Comonad instead of a Functor, for that we need something of the type
-- cfix :: Comonad w => w (w a -> a) -> w a
-- there is wfix which is similar but that cannot be expressed in terms of fix on its argument. So it's inefficient "because there is no single reference to the eventual future of the computation". What exactly does that mean?
-- he says there is no sharing so computing the sum up to n for each step it has to compute the sum from the beginning, which is quadratic
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (fmap wfix $ duplicate w)

-- nats = iterate (+1) 0
-- integers = iterateTape (subtract 1) (+1) 0

-- this evaluate is more efficient, but needs the ComonadApply instance
-- Foner: "We can take the space of functions that represent the space of values and turn it into a space of values in a way that is maximally sharing"
-- evaluate :: Comonad w => w (w a -> a) -> w a
-- evaluate fs = fix $ (fs <@>) . duplicate

data RefType = Relative | Absolute
-- the t is used to give the type Ref more information to be able to constrain what kind of instances of CombineRef can be written using the type family Combine
data Ref (t :: RefType) where
  Rel :: Int -> Ref Relative
  Abs :: Int -> Ref Absolute

-- does not work b/c Ref's kind seems to be too specific. Needs to be * -> * but is RefType -> *. Maybe it does not need a Functor instance for the ConicList but just this shape of kind
-- instance Functor Ref where
--   fmap f (Rel i) = Rel $ f i
--   fmap f (Abs i) = Abs $ f i

-- use a type family to constrain the implementations of a type class
-- Foner: "The closedness of a type class can be used to infect a type class, which themselves are technically open"
type family Combine a b where
  Combine Relative Absolute = Absolute
  Combine Absolute Relative = Absolute
  Combine Relative Relative = Relative
  
class CombineRefs a b where
  combine :: Ref a -> Ref b -> Ref (Combine a b)

instance CombineRefs Absolute Relative where
  combine (Abs i) (Rel j) = Abs (i + j)

  
data x :-: y
data Nil

-- Also called TList or FList
-- A heterogenous list where all the elements lives inside a Functor
data ConicList f ts where
  (:-:) :: f x -> ConicList f xs -> ConicList f (x :-: xs)
  ConicNil :: ConicList f Nil
infixr 4 :-:

-- so a list of references is a heterogenous list of either relative or absolute references and in Type we have a type-level list to tell which things are absolute or relative
type RefList = ConicList Ref

type family a & b where
  (a :-: as) & (b :-: bs) = Combine a b :-: (as & bs)
  Nil & bs = bs
  as & Nil = as

class CombineRefLists as bs where
  (&) :: RefList as -> RefList bs -> RefList (as & bs)

instance CombineRefLists Nil Nil where
  ConicNil & ConicNil = ConicNil

-- a two dimensional reference
-- refa :: RefList (Relative :-: Relative :-: Nil)
-- refa = belowBy 3 & rightBy 14

-- He can use his Sheet framework to model Game of Life (3 dimensions with two being space and one time which always references the last iteration) and the Waterflow problem
