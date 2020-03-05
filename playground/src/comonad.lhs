> import Control.Comonad

From Edward Kmetts presentation, a Comonad instance for a product type.

> data Product e a = Product e a
> instance Functor (Product e) where
>   fmap f (Product e a) = Product e $ f a
> instance Comonad (Product e) where
>   extract (Product e a) = a
>   duplicate (Product e a) = Product e (Product e a)
