> {-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}
> import Control.Monad.Reader
> import Control.Monad.State

Add type operators for the :^ notation
Unfortunately we can't use the ~ notation to express that () :^ Bool is equal to Bool -> () as he does in the tutorial. GHC complains that it found something with kind `Constraint` instead of a type, what is that?

> type b :^ a = a -> b
> fnBoolUnit :: () :^ Bool
> fnBoolUnit _ = ()

He regards any * -> * kinded type as a "Context". For example Identity, List, etc.

A Functor context allows you to access the contained type but not to change the context

> fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
> fmapMaybe f Nothing = Nothing
> fmapmaybe f (Just x) = Just $ f x

> fmapReader :: (a -> b) -> Reader r a -> Reader r b
> fmapReader f readFn = reader (\r -> f $ runReader readFn r)

> fmapState :: (a -> b) -> State s a -> State s b
> fmapState f stateFn = state (\s -> let (a, s') = runState stateFn s in (f a, s'))

The f function in all of these functors is completely unaware of the context. It’s not allowed to alter the context, and it’s not allowed to be informed by the context.

> applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
> applyMaybe (Just f) m = fmap f m
> applyMaybe Nothing _ = Nothing

With Functor, we definitely had to have a function to fmap over the value. With Applicative, we can potentially do Nothing <*> Just 'a', which results in Nothing. We’ve gained a way to change the structure!

Let’s consider List, now. We have two bits of information in the context of the list: the number of elements in a list, and the order of elements in the list. If we use the number of elements in the list as our extra information, then we can combine the number of elements in some way in the result list. If we use the order of elements, then we can zip the two lists together with function application, pairing the function at index i with the value at index i.

> newtype ZipList a = ZipList { unZipList :: [a] } deriving (Functor)
> instance Applicative ZipList where
>   pure a = ZipList (repeat a)
>   ZipList [] <*> _ = ZipList []
>   _ <*> ZipList [] = ZipList []
>   ZipList (f:fs) <*> ZipList (x:xs) = ZipList (f x : (fs <*> xs))

ZipList allows us to modify the structure if we e.g. use a shorter list of functions than the list of values it gets truncated.
This ZipList Applicative uses the order of the list. We could also do something with the length of the list.

We could do either Addition or Multiplication. He explains that the resulting instance also needs to form a Monoid (which + and * do with 0 and 1). So if + is our operation pure needs to result in a unit length list, i.e. list of length 0. pure _ = Nil. But that does not satisfy the Applicative laws.

So we will rather use multiplication.

> newtype MultList a = MultList { unMultList :: [a] } deriving (Functor, Show)
> instance Applicative MultList where
>   pure a = MultList [a]

The first two pattern correspond to multiplying with 0

>   MultList [] <*> _ = MultList []
>   _ <*> MultList [] = MultList []

If we have just one function we fmap it over the other list to satisfy the Applicative laws. But this is also handled the same way in the case of multiple functions below.

>   -- MultList (f:[]) <*> MultList as = MultList $ fmap f as

If there is more than one function we map each function over all values and concatenate the results. This results in a list that has size |fs| * |as| so it multiplies the lengths of the input lists

>   MultList fs <*> MultList as = MultList $ concatMap (\f -> fmap f as) fs

> x1 = MultList [(* 2), (* 3), (+ 1)] <*> MultList [1,2]


What about exponentiation with ^1 as a unit? Nope, exponentiation is not associative :(

The Reader Applicative instance allows you to apply functions that depend on an environment to values that depend on an environment. The intuition would be a set of plans that all depend on the same decision.
For example the line Plan <$> chooseTransport maps the Plan constructor over the chooseTransport Reader so that what's left is a `Reader Weather (Clothing -> Plan)`.
In the end, the chooseTransport and chooseClothing functions will get access to the same environment when the reader is run. This would allow us to execute all these in parallel but also means that we can't have any dependencies, like never wearing shorts if we take a bike.

> data Weather = Sunny | Raining | Cold
> data Transport = Bike | Car
> data Clothing = BigCoat | Jacket | StretchyPants
> 
> chooseTransport :: Reader Weather Transport
> chooseTransport =
>   reader (\env ->
>     case env of 
>          Sunny -> Bike
>          Cold -> Bike
>          Raining -> Car
>   )
> 
> chooseClothing :: Reader Weather Clothing
> chooseClothing =
>   reader (\env ->
>     case env of
>          Sunny -> StretchyPants
>          Raining -> Jacket
>          Cold -> BigCoat
>   )
> 
> data Plan = Plan Transport Clothing
> 
> whatDo :: Weather -> Plan
> whatDo =
>   runReader (Plan <$> chooseTransport <*> chooseClothing)

The State Applicative is similar to the Reader Applicative but since State s a = s -> (a, s) we don't have to give each function the same input but can weave the state through the applies like this
  (<*>) :: State s (a -> b) -> State s a -> State s b
  sf <*> sa =
    State (\s ->
      let (f, s')  = runState sf s
          (a, s'') = runState sa s'
       in (f a, s'')
    )
So we use the first state to get the function and a new state and then the new state to get the value. This allows us to write an apply chain that where something depends on the previous value. As a consequence we cannot easily parallelize these.

A summary:
A functor is a context where we can map over, but we can’t look at the context, and we can’t change the context. An Applicative allows our contexts to interact independently of the values contained in the contexts.

What if we want to alter the context based on the values produced by the computation? We use Monads.

fmap lifts a normal function into the context. But it can only operate on the value, it's not aware of the context. With <*> we can put the function into the context so it has a bit more information.
fmap ::   (a -> b) -> f a -> f b
<*>  :: f (a -> b) -> f a -> f b

If we instead have a function a -> f b that takes an a and then returns a b with context and we want to use it like in fmap we need a way to collapse a nested context. This results in a Monad.

Why can't we write a Monad instance for Validation e?

instante Monad (Validation e) where
  Correct a >>= f = f a
  Errors e >>= f = ...

Since we don't have a value we cannot apply f so the only option is to short-circuit but then we just have the Either behavior. We would like to collect all Errors but that is not possible since a Monad requires the next step to depend on the last.


