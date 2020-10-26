> {-# LANGUAGE RankNTypes #-}

> data Nested a = NestedCons a (Nested [a]) | NestedNil

> foo :: (forall b. b -> Int) -> Nested a -> Int
> foo f NestedNil = f NestedNil
> foo f n@(NestedCons a ns) = (f n) + foo f ns

> test = NestedCons 1 (NestedCons [2, 3] NestedNil)

I'm having trouble thinking of any interesing function argument for foo because it will need to operate on a's and then list of a's and so on. Maybe y supplying a typeclass that [] implements as well as a.
