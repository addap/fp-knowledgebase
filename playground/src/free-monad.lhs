> {-# LANGUAGE StandaloneDeriving, UndecidableInstances, ExistentialQuantification, DeriveFunctor #-}
> 
> module Free where
>
> import Control.Monad
> import Prelude hiding (and)

This implements a Free Monad.

Afaik there are two possible implementations for the Free Monad datatype, one with Bind and one with Suspend. It's probably related to how you can implement Monads with either bind or join.

This is the first implementation using Suspend.

> data Free f a = Pure a | Suspend (f (Free f a))
>
> deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)
> --instance (Show a, Show (f (Free f a))) => Show (Free f a) where
> --  show (Pure a) = "Pure " ++ show a
> --  show (Suspend fa) = "Suspend (" ++ (show fa) ++ ")"

The Pure case is trivial. In the Suspend case we need to fmap twice, once over the Functor f and then over the Functor (Free f) inside.

> instance Functor f => Functor (Free f) where
>   fmap g (Pure a) = Pure (g a)
>   fmap g (Suspend f) = Suspend (fmap (fmap g) f)

The Applicative instance is also straightforward. In the Pure case we can just use fmap. In the Suspend case we need to use recursion I think (actually just like in the Functor case if you think about it) b/c the Suspend constructor itself is recursive.

> instance Functor f => Applicative (Free f) where
>   pure = Pure
>   (Pure g) <*> as = fmap g as
>   (Suspend fg) <*> as = Suspend $ fmap (<*> as) fg

We can try to implement the cases by hand, too.
fmap over the fa and then fmap over each Free contained within.
(Pure f) <*> (Suspend fa) = Suspend $ fmap (fmap f) fa

We map over the f Functor and then over each Free inside f and apply the a to the function contained within.
(Suspend f) <*> (Pure a) = Suspend $ fmap (fmap ($ a)) f

I tried to write this whithout recursion using just fmap but that will try to construct the infinite type b ~ f (Free f b)
(Suspend f) <*> (Suspend faf) = Suspend $ fmap (fmap (\f' -> fmap (fmap f') faf)) f

To define the Monad instance of (Free f) we can first define an analog to join on (Free f). If you substitute (Free f) = m this has the same signature.
joinFree :: m (m a) -> m a

> joinFree :: Functor f => Free f (Free f a) -> Free f a
> joinFree (Pure a) = a
> joinFree (Suspend f) = Suspend $ fmap joinFree f

The Monad instance can then define >>= using the standard way of fmap and join

> instance (Functor f) => Monad (Free f) where
>   return = Pure
>   (Pure a) >>= g = g a
>   f >>= g = joinFree $ fmap g f

To lift a value from the functor into the free monad we have liftFree which maps over the functor turning all the a's into Free f a's and putting all that into a Suspend.

> liftFree :: Functor f => f a -> Free f a
> liftFree fa = Suspend $ fmap Pure fa

We can also "interpret" the whole structure using foldFree. This is exactly the same definition as in the Haskell standard library.

> foldFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b

The Pure case (an consequently the r) are never used with BoolAlg since fmapping over BoolAlg never does something, so the liftFree never actually applies Pure to anything.

> foldFree r g (Pure a) = r a
> foldFree r g (Suspend fa) = g (fmap (foldFree r g) fa)


I first tried defining the boolean algebra from Runar's talk like this but this is wrong. I though I needed explicit recursion in the And case but I don't. That's what the free monad is there for, it builds the fixed point of the type.
A type like this would not lead to an interesting free monad because it does not compose since none of the constructors have an `a` which is where the link to the rest of the program is.

> data BoolAlg a = Lit Bool | And (BoolAlg a) (BoolAlg a) | Var String deriving (Show)


This works without the explicit recursion in the type but it's still not very interesing. And I find the branching behavior that occurs since the And2 case has two `a` not appropriate for the boolean formulas here. e.g. in formula2 the last `lit2 False` gets mapped over the preceding expression so they all have a (Lit2 False (Pure ())) at the leafs.
Also, what exactly is happening inside the and2?
I'm not sure how to interpret one of these either. The only way would be if there is only one statement in the do block so that the weird branching behavior does not appear but in that case the whole construction does not make sense to begin with.

> data BoolAlg2 a = Lit2 Bool a | And2 a a deriving (Functor, Show)
> type BoolExp2 a = Free BoolAlg2 a
>
> lit2 :: Bool -> BoolExp2 ()
> lit2 b = liftFree $ Lit2 b ()
> and2 :: (BoolExp2 a) -> (BoolExp2 a) -> BoolExp2 a
> and2 t1 t2 = do
>   joinFree $ liftFree $ And2 t1 t2
> formula2 :: BoolExp2 ()
> formula2 = do
>   and2
>     (do and2 (lit2 True) (lit2 True))
>     (lit2 True)
>   lit2 False
          
If I try it with one a I get a "nice" imperative syntax and I can even write an interpreter for it that uses a stack of previous results (idea from the "Compile Time Parsing" functional pearl paper but without type safety)

> data BoolAlg3 a = Lit3 Bool a | And3 a | Or3 a deriving (Functor, Show)
> type BoolExp3 a = Free BoolAlg3 a
>
> lit3 :: Bool -> BoolExp3 ()
> lit3 b = liftFree $ Lit3 b ()
> and3 :: BoolExp3 ()
> and3 = liftFree $ And3 ()
> or3 = liftFree $ Or3 ()
>
> formula3 :: BoolExp3 ()
> formula3 = do
>   and3
>   or3
>   lit3 False
>   lit3 False
>   lit3 True
>
> evalBool3 :: BoolExp3 () -> Bool
> evalBool3 fm = eval fm [False] !! 0
>   where eval :: BoolExp3 () -> [Bool] -> [Bool]
>         eval (Pure _) [] = []
>         eval (Pure _) bs = False <$ bs
>         eval (Suspend (Lit3 b next)) (_:br) =
>           let rs = eval next br
>           in b : rs
>         eval (Suspend (And3 next)) (_:br) =
>           let (t1:t2:rs) = eval next (False : False : br)
>           in (t1 && t2) : rs
>         eval (Suspend (Or3 next)) (_:br) =
>           let (t1:t2:rs) = eval next (False : False : br)
>           in (t1 || t2) : rs
>         


This might be how the other definition (from Runar Bjarnason's talk Composable application architecture with reasonably priced monads) of the Free Monad would look like. He uses scala where you can apparently use a new type variable in constructors, I had to use forall i here but I have no idea what the consequences of that are.

data Free2 f a = Pure2 a | forall i. Bind (f i) (i -> Free2 f a)
