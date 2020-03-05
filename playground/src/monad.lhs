> module MonadIntuition where

I don't now how to write an Applicative instance for a binary tree.

> data Tree v = Leaf v | Branch (Tree v) (Tree v) deriving (Show)
> instance Functor Tree where
>   fmap f (Leaf v) = Leaf $ f v
>   fmap f (Branch t1 t2) = Branch (f <$> t1) (f <$> t2)
> instance Applicative Tree where
>   pure = Leaf
>   Leaf f <*> vs = fmap f vs
>   (Branch f1 f2) <*> vs = f1 <*> vs
> --instance Monad Tree where
> --  return = Leaf
>   
> x = 1
