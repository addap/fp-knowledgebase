> {-# LANGUAGE StandaloneDeriving, UndecidableInstances, ExistentialQuantification #-}
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

Try out the Free Monad Interpreter example using Formulas.
We first define our boolean algebra and then boolean expressions using the algebra and Free.

> data BoolAlg a = Lit Bool | And (BoolAlg a) (BoolAlg a) | Var String deriving (Show)

I need the a to make it a functor but it's actually never used. Is that a Phantom Type? Apparently not because Phantom Types don't appear at all on the right hand side but the a does, but it's still never used. Normally you would use it to hold the next step of the computation but our formulas don't have this imperative concept.

> instance Functor BoolAlg where
>   fmap f (Lit b) = Lit b
>   fmap f (And e1 e2) = And (fmap f e1) (fmap f e2)
>   fmap f (Var v) = Var v
>
> type BoolExp a = Free BoolAlg a
>
> evalFormula :: (String -> BoolAlg a) -> BoolAlg a -> Bool
> evalFormula _ (Lit b) = b
> evalFormula env (And e1 e2) = (evalFormula env e1) && (evalFormula env e2)
> evalFormula env (Var v) = evalFormula env $ env v

> printFormula :: (String -> BoolAlg a) -> BoolAlg a -> String
> printFormula _ (Lit b) = show b
> printFormula env (And e1 e2) = "(" ++ (printFormula env e1) ++ " && " ++ (printFormula env e2) ++ ")"
> printFormula env (Var v) = "'" ++ v ++ "'=" ++ (printFormula env $ env v)
>
> negFormula :: BoolAlg a -> BoolAlg a
> negFormula (Lit b) = Lit $ not b
> negFormula (And e1 e2) = And (negFormula e1) (negFormula e2)

> formula :: BoolAlg a
> formula = And (Lit True) (Var "x")
> formulaF :: BoolExp a
> formulaF = liftFree formula
> env :: String -> BoolAlg a
> env = (\x -> if x == "x" then And (Var "y") (Lit True) else Lit True)
> x1 = foldFree id (evalFormula env) formulaF
> x2 = foldFree id (printFormula env) formulaF
> x3 = negFormula formula
>

I don't use the a in the BoolAlg constructors directly, only as a means to write a Functor instance. In his `Interpreter Pattern Revisited` talk, Runar Bjarnason talks about how you could use that a as identifiers for variables. But when I try to add that (a = String and adding env arguments to the eval and print functions) it does not work since my foldFree takes an (f b -> b) argument, so the a always has to be the same as the result type so evalFormula :: BoolAlg String -> Bool does not work. I think that's an error on Runar's part, or things work differently in Scala than in Haskell. 

If I define smart constructors for my algebra here, they all behave like the `done` case in the free-monad2.lhs example. Since none of them wrap an `a` you can't compose them.
Something like this would be practical
do x <- lit True
   y <- and x x
   return y
I can already do it a little bit by using the correct constructors instead of the <- syntax but I'm wondering if each of the Constructors could pass "itself" to the next bound function.

> lit b = liftFree (Lit b)
> and t1 t2 = liftFree (And t1 t2)
> var s = liftFree (Var s)
> test :: BoolExp a
> test = do
>   let x = Lit True
>   and x x


This might be how the other definition (from Runar Bjarnason's talk Composable application architecture with reasonably priced monads) of the Free Monad would look like. He uses scala where you can apparently use a new type variable in constructors, I had to use forall i here but I have no idea what the consequences of that are.

> data Free2 f a = Pure2 a | forall i. Bind (f i) (i -> Free2 f a)
