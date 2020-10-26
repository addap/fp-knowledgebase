{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

import Control.Comonad.Store
import Control.Monad.Identity


-- we can define lenses as a coalgebra store comonad
type Coalg f a = a -> f a

type Lens1 a b = Coalg (Store b) a

(>-+) :: Lens1 a b -> Lens1 b c -> Lens1 a c
la >-+ lb = \pieceA -> let (holeBA, pieceB) = runStore $ la pieceA
                           (holeCB, pieceC) = runStore $ lb pieceB
                       in store (holeBA . holeCB) pieceC

l1 :: Lens1 (a, b) a
l1 (a, b) = store (\piece -> (piece, b)) a

-- we can read using the lens
x1 = pos $ l1 (1, 2)
-- and update
x2 = peek 4 $ l1 (1, 2)

-- we could compose the tuple lens directly
x3 = pos $ (l1 >-+ l1) ((3, 4), 5)
x4 = peek 13 $ (l1 >-+ l1) ((3, 4), 5)


-- or we use the isomorphism between the store comonad and ((b -> f b) -> f a) and write it like this
-- also I'm going to call the type variables b "big" and s "small" because the a, b confuse me
type Lens2 b a = forall f. Functor f => Coalg f a -> Coalg f b

-- for the getter we build a trivial function that maps s to the "identity-store" Store s s
-- then we just get a Store s b and with runStore we can get the s back. So it's basically just
-- an elaborate scheme to get access to the s that gets passed to the lambda here
get2 :: Lens2 b s -> b -> s
get2 l b = snd $ runStore $ l (\s -> store id s) b
  
-- set2 :: Lens2 b s -> b -> s -> b
-- set2 l b s = l (\

(>-) :: Lens2 a b -> Lens2 b c -> Lens2 a c
la >- lb = la . lb

-- but how do I write a simple lense for the first element of a tuple?
-- here the a gets passed to our injection function (e.g. the \s -> ... from get2 above)
-- and we map over it to change the type of the functor
l2 :: Lens2 (a, b) a
l2 inj (a, b) = (,b) <$> inj a


data Small a = Small a a
data Middle a = Middle (Small a) (Small Int)
data Big = Big (Middle String) (Middle Int)

b = Big (Middle (Small "hello" "world") (Small 1 2))
    (Middle (Small 13 37) (Small 3 4))

