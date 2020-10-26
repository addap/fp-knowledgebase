> import Data.Functor.Extend
> import Control.Comonad

Store consists of a value s and a function to look up a's if you have s's.
Store s a is literally a store/warehouse of a's accessed by s's with a read-head positioned at a particular s. The function s -> a is the lookup and the value s is the read head. If s ~ Int this seems isomorphic to Tape from the Comonad talk.

> data Store s a = Store (s -> a) s
> instance Functor (Store s) where
>   fmap f (Store g s) = Store (f . g) s
> instance Extend (Store s) where

you can see in the duplicate implementation that the lookup function of the duplicated Store just maps the input s' to the original Store centered around that s'. It's like the Tape example where each cell then contains a new Tape centered around the previous cell.
 
> duplicated (Store g s) = Store (\s' -> Store g s') s
> instance Comonad (Store s) where
>   extract (Store g s) = g s
>   duplicate = duplicated

For a comonad we have `extend f = fmap f . duplicate` so extending is kind of more powerful than a simple fmap since in that case the function gets access to the whole structure to update the value at a particular slot as `duplicate :: w a -> w (w a)` so you have the whole structure in every slot.

