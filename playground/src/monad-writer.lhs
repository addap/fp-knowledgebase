The Writer monad is just a pair of a monoid w and the result a

> newtype Writer w a = Writer { runWriter :: (w, a) }
>
> instance (Monoid w) => Functor (Writer w) where
>   fmap f (Writer (w, a)) = Writer (w, f a)

I'm not sure if <*> should ignore the log in the first Writer. No it should not because of the 3rd Applicative Law (Interchange, u <*> pure y = pure ($ y) <*> u). If we ignore the log of the function we don't fulfill that law.

> instance (Monoid w) => Applicative (Writer w) where
>   pure a = Writer (mempty, a)
>   Writer (fw, f) <*> Writer (w, a) = Writer (fw <> w, f a)
> 
> instance (Monoid w) => Monad (Writer w) where
>   return = pure
>   Writer (w, a) >>= k =
>     let (w', a') = runWriter $ k a
>     in Writer (w <> w', a')

Helper functions to work with Writer.
tell just writes a log with a boring () value

> tell :: (Monoid w) => w -> Writer w ()
> tell w = Writer (w, ())

listen adds the current log to the output

> listen :: (Monoid w) => Writer w a -> Writer w (a, w)
> listen (Writer (w, a)) = Writer (w, (a, w))

> logThing :: (Show a) => a -> Writer [String] a
> -- logThing x = Writer (["Logging a <" ++ show x ++ ">"], x)
> logThing x = do
>   tell ["Logging a <" ++ show x ++ ">"]
>   return x
>
> multWithLog :: Writer [String] Int
> multWithLog = do
>   a <- logThing 3
>   b <- logThing 5
>   return (a * b)

> x = 1
