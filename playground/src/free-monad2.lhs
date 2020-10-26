> {-# LANGUAGE StandaloneDeriving, UndecidableInstances, DeriveFunctor, EmptyDataDeriving, Rank2Types #-}
> module YouCouldHaveInventedFreeMonads where
> import Control.Monad.Writer

The datatype for our toy language consists of outputting stuff, calling a bell and an end marker. The next type variable is used to carry the rest of the program.

> data Toy b next = Output b next | Bell next | Done deriving (Show, Functor)

You can already write programs in it, but the type changes if you add more commands.

> x1 = Output "A" Done
> x2 = Output "A" (Bell Done)

But we can use the Fix data type (which is the fixed point of a functor) to make it more tractable. The Fix constructor is pretty much the same as the Suspend constructor from the Free Monad, it's just missing the a.

> data Fix f = Fix (f (Fix f))
> deriving instance (Show (f (Fix f))) => Show (Fix f)

With that we can write the same programs.

> x3 = Fix (Output "A" (Fix Done))
> x4 = Fix (Output "A" (Fix (Bell (Fix Done))))

But something like this does not work since the b's must all be the same type and every program must be terminated with Done which is pretty impractical. 
Fix (Output "A" (Fix (Output 3 (Fix Done))))
x5 = Fix (Output "A" ???)

To remedy this we could add another case to Fix: Throw e, which leads to the Free Monad (the b must still be the same everywhere unfortunately). We can then use the do notation to structure our toy language. But since the Monad we're working on is `Free (Toy b)`, not `Toy b` we can't use our functions directly but have to lift them first.
The Free Monad is organized like a list (or rather a tree). The Pure case denotes the end to the computation and the Impure case the next step in the computation. It can be a tree since the Impure case contains the functor which could have multiple branches if its constructor uses the type variable several times. But with the toy language here it degenerates to a list.

> data Free f a = Pure a | Impure (f (Free f a))
> deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)

The Functor instance of Free is used to apply the function to the leafs (or end of the list). In the Impure case it just fmaps twice to get to the next level of the Functor, i.e. one fmap is used to just skip over the f functor.

> instance (Functor f) => Functor (Free f) where
>   fmap f (Pure a) = Pure (f a)
>   fmap f (Impure ffa) = Impure $ fmap (fmap f) ffa

The Pure case of apply we just fmap the function over the other Free monad which in turn just applies the function to the leafs (or end of the list). In the Impure pass the apply itself on to the next step of the computaiton. Intuitively, if we have a computation that results in function we can apply it to a computation that results in values and we will "graft" the tree of values onto each leaf of functions and apply their leaves to the function.

> instance (Functor f) => Applicative (Free f) where
>   pure = Pure
>   (Pure f) <*> as = fmap f as
>   (Impure ffa) <*> as = Impure $ fmap (<*> as) ffa

joinFree basically appends nested trees/lists of actions. If you have one Free Monad with another as its return type it means all the leaves of the first contain free monads. This replaces all these leaves in the first one by what they contain.

> joinFree :: (Functor f) => Free f (Free f a) -> Free f a
> joinFree (Pure a) = a
> joinFree (Impure ffa) = Impure $ fmap joinFree ffa

The >>= then simply allows us to map a function over a computation that has access to the return value and returns another computation where the nested computations are flattened. But this actually does nothing, it just builds up this ever longer tree of Free which can then be interpreted by some function.

> instance (Functor f) => Monad (Free f) where
>   return = pure
>   (Pure a) >>= f = f a
>   m >>= f = joinFree $ fmap f m
> 
> liftF :: (Functor f) => f a -> Free f a
> liftF fa = Impure $ fmap Pure fa

I can have the output also return the string it printed. If I set the `next` part of a constructor of Toy I can control what is returned by the computation.

> output :: a -> Free (Toy a) a
> output x = liftF (Output x x)
> bell :: Free (Toy a) ()
> bell = liftF (Bell ())

Using a dummy datatype we can mark the done command as terminating a program so that the eval procedure later only accepts something of the form `Free (Toy b) Terminated`
a.d. can we also ensure that you can't bind to a done? so that the program with a bell after done does not even typecheck.

> done :: Free (Toy a) r
> done = liftF Done
>
> data Terminated = Terminated deriving (Show)

We can now use the do notation to structure our programs.

> subroutine :: Free (Toy String) String
> subroutine = output "A"
>
> program :: Free (Toy String) r
> program = do
>   x <- subroutine
>   y <- output x
>   bell
>   done

I can add more but it does not change the program.

>   bell
>   done

Notice how the second bell after the first done is never interpreted. That's because the Free Monad is like a tree with the constructors of the underlying functor as its leaves and Done does not contain anything so everything else gets thrown away in the bind after done.

> showProgram :: (Show a, Show r) => Free (Toy a) r -> String
> showProgram (Impure (Output a x)) =
>     "output " ++ show a ++ "\n" ++ showProgram x
> showProgram (Impure (Bell x)) =
>     "bell\n" ++ showProgram x
> showProgram (Impure Done) =
>     "done\n"
> showProgram (Pure r) =
>     "return " ++ show r ++ "\n"
>
> pretty :: (Show a, Show r) => Free (Toy a) r -> IO ()
> pretty = putStr . showProgram

showProgram is one interpreter of our toy language. But we can also write a conventional interpreter for it.

> evalToy :: (Show a, Show r) => Free (Toy a) r -> IO ()
> evalToy (Impure (Output x next)) = print x >> evalToy next
> evalToy (Impure (Bell next)) = print "RING!" >> evalToy next
> evalToy (Impure Done) = print "DONE" >> return ()
> evalToy (Pure a) = print $ "Improper termination of program with " ++ show a

Try to write the same interpreter using the foldFree function. The foldFree function is the actual interpreter/can be used to write interpreters for the Free Monad. It knows how to interpret the Pure's at the leafs of the computation and for Impure's it knows how to interpret their contained functor if its children have already been interpreted.
You can actually use the foldFree thing to fold the free monad into another monad if I instantiate b with this other monad. This can also be another Free Monad wrapping a different language. So it can be a compiler between the languages represented by the Free Monads.


> foldFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
> foldFree r _ (Pure a) = r a
> foldFree r g (Impure fa) = g (fmap (foldFree r g) fa)

Can use `Free (Toy a) Terminated` here so that only temrianted programs typecheck.

> -- evalToy' :: (Show a) => Free (Toy a) Terminated -> IO ()
> evalToy' :: (Show a) => (forall r. Free (Toy a) r) -> IO ()
> evalToy' prg = foldFree pur imp prg
>   where
>     pur :: r -> IO ()
>     pur = \a -> print $ "Improper termination of program"
>     imp :: (Show a) => (Toy a (IO r)) -> IO ()
>     imp (Output x next) = print x >> next >> return ()
>     imp (Bell next) = print "RING!" >> next >> return ()
>     imp (Done) = print "DONE"
>
> x8 = evalToy' program
> --x9 = evalToy' subroutine

using the (forall r) quantification x9 does not compile anymore. Because the only way we can construct a Free (Toy a) r is by using the `done` function, i.e. evalToy' only accepts well-formed programs. To understand the why I need to learn what Rank2Types/RankNTypes and System F are. The gist seems to be that evalToy' can now decide how to instantiate r, not the called of evalToy'. That means that passing in something like subroutine with r = String is not allowed because r is already assigned.

Let's try to write an interpreter that does not use IO but just the writer monad.

> writeToy :: (Show a) => Free (Toy a) Terminated -> Writer [String] Terminated
> writeToy prg = foldFree pur imp prg
>   where
>     pur :: Terminated -> Writer [String] Terminated
>     pur = \a -> (tell $ ["Improper termination with " ++ show a]) >> return a
>     imp :: (Show a) => Toy a (Writer [String] Terminated) -> Writer [String] Terminated
>     imp (Output x w) = tell [show x] >> w
>     imp (Bell w) = tell ["RING!"] >> w
>     imp Done = (tell ["DONE"]) >> return Terminated
>
> x10 = execWriter $ writeToy program

Now I begin to understand what Runar Bjarnason meant by translating the language into another monad. If I use the IO monad I can print stuff directly and when I use the Writer monad I record the output of the program.
But that's still missing a way to run the program and turn it into a single value like the showProgram which just outputs a string. Maybe you could use the Identity Monad instead?

Try out the abstraction of translating monads from "Interpreter Pattern Revisited".

foldMap' :: (Monad m) => ((Toy a r) -> m r) -> (Free (Toy a) r) -> m r
foldMap' _ (Pure a)  = return a
--foldMap' trans (Impure ffa) = trans ffa >>= (foldMap trans)
foldMap' trans (Impure ffa) = trans $ fmap _ ffa

I don't know how to construct the equivalent of (f: F ~ G) in Haskell. I tried the trans function but that leads to an infinite type.


--- Part 2 Concurrency

We can view a Thread as a list of monadic actions. To order them and to be able to pass the result of an action to the next we can organize them in a list. This is the same datatype as the Free Monad.

> data Thread m r = Atomic (m (Thread m r)) | Return r
> deriving instance (Show r, Show (m (Thread m r))) => (Show (Thread m r))

The Atomic constructor wraps one single step and the Return is used as an end marker. We can turn any single monad invocation into an atomic Thread step.

> atomic :: (Monad m) => m a -> Thread m a
> atomic m = Atomic $ liftM Return m

The atomic procedure is similar to liftF but it requires a monad and not just a functor.

> instance (Monad m) => Functor (Thread m) where
>   fmap f (Return x) = Return $ f x
>   fmap f (Atomic ffa) = Atomic $ fmap (fmap f) ffa
> instance (Monad m) => Applicative (Thread m) where
>   pure = Return
>   (Return f) <*> as = fmap f as
>   (Atomic f) <*> as = Atomic $ fmap (<*> as) f
> instance (Monad m) => Monad (Thread m) where
>   return = Return
>   (Atomic m) >>= f = Atomic $ liftM (>>= f) m
>   (Return r) >>= f = f r
>
> thread1 :: Thread IO ()
> thread1 = do
>   atomic $ print 1
>   atomic $ print 2
>   atomic $ print 3
>
> thread2 :: Thread IO ()
> thread2 = do
>   str <- atomic $ getLine
>   yield
>   atomic $ putStrLn $ "got : " ++ str

The recursive steps pops off the first action from both threads and gets the next action in `next1`, `next2` and then interleaves them.

> interleave :: (Monad m) => Thread m r -> Thread m r -> Thread m r
> interleave (Atomic m1) (Atomic m2) = do
>   next1 <- atomic $ m1
>   next2 <- atomic $ m2
>   interleave next1 next2
> interleave t1 (Return _) = t1
> interleave (Return _) t2 = t2
>
> runThread :: (Monad m) => Thread m r -> m r
> runThread (Return r) = return r
> runThread (Atomic m) = m >>= runThread

A comment on the blog mentioned the yield function like this. This would cause a thread to pause one step.

> yield :: (Monad m) => Thread m ()
> yield = atomic $ return ()

a.d. try to write an interpreter that schedules threads for a maximum number of steps but also switches on yield.

> data ThreadAction = Continue | Yld
> data ThreadAlg a = TPrint String a | TRead (String -> a) | Yield a deriving (Functor)
> instance (Show a) => Show (ThreadAlg a) where
>   show (TPrint s a) = "TPrint " ++ show s ++ " (" ++ show a ++ ")"
>   show (TRead f) = "TRead (->" ++ show (f "") ++ ")"
>   show (Yield a) = "Yield (" ++ show a ++ ")"
> type ThreadLang = Free ThreadAlg
> 
> prt :: (Show a) => a -> ThreadLang ()
> prt x = liftF $ TPrint (show x) ()
>
> rd :: ThreadLang String
> rd = liftF $ TRead id
>
> yld :: ThreadLang ()
> yld = liftF $ Yield ()
>
> t1 :: ThreadLang ()
> t1 = do
>   prt 1
>   prt 2
>   yld
>   prt 3
>   yld
>   prt 4
>
>
> t2 :: ThreadLang ()
> t2 = do
>   prt "A"
>   prt "B"
>   yld
>   prt "C"
>
> t3 :: ThreadLang ()
> t3 = forever $ do
>   prt "I am t3. Should I yield"
>   s <- rd
>   if (s == "yes")
>     then do prt "yielding t3"
>             yld
>             prt "waking up t3"
>     else prt "not yielding"

The forever makes a computation run forever by applying it to itself. Something like `forever a = let a' = a *> a' in a`. If I used forever with one of the branching programs it would probably diverge.

> t4 :: ThreadLang ()
> t4 = forever $ do
>   prt "I am t4. Should I yield"
>   s <- rd
>   if (s == "yes")
>     then do prt "yielding t4"
>             yld
>             prt "waking up t4"
>     else prt "not yielding"

The scheduling works by matching on the Yield constructor. If there is a yield we just run the other thread and switch their order. Otherwise we always run the first thread.
This implementation of interleave' removes all the yields, transforming the program into a linear sequence of steps. (But what if I yield based on input? Then it still removes all yields because it's lazy!)
You could also use the commented out code to keep the yields but they are ignored by the interpreter anyways.

> interleave' :: ThreadLang r -> ThreadLang r -> ThreadLang r
> interleave' (Impure (Yield m1)) m2 = do
>   --next1 <- liftF $ Yield m1
>   interleave' m2 m1
> interleave' (Impure m1) m2 = do

the thing with liftF m1 works because it creates a Free monad that has just the next action at the root (or rather first list item) and the next ones are all wrapped in a Pure, so they get bound to the next1.
i.e. it turns a sequence of actions like this (TPrint 1 (Impure TPrint 2 ...)) into this (Impure (TPrint 1 (Pure (Impure TPrint 2 ...)))) so that next1 gets bound to (Impure TPrint 2 ...).
The whole thing is in a do-block so this case of interleave' would add the sequence represented by liftF m1 (i.e. only the  TPrint 1 action) to the interleaved sequence of actions which is exactly what should happen.

>   next1 <- liftF m1
>   interleave' next1 m2
> interleave' (Pure a) (Impure m1) = interleave' (Impure m1) (Pure a)
> interleave' (Pure a) (Pure _) = Pure a

Removes all yields from a program by interleaving with the empty Thread. I could probably do it for arbitrary threads, not just ones that return () by writing more cases in interleave' so that in the end it always returns the Pure of the first thread.

> removeYields :: ThreadLang () -> ThreadLang ()
> removeYields t = interleave' t (Pure ())
> 
>
> int1 = interleave' t1 t2
> int2 = interleave' t2 t1
> int3 = interleave' t3 t4
> 
> runThread' :: ThreadLang r -> IO r
> runThread' prg = foldFree pur imp prg
>   where
>     pur :: r -> IO r
>     pur = return
>     imp :: ThreadAlg (IO r) -> IO r
>     imp (TPrint s next) = putStrLn s >> next
>     imp (TRead next) = getLine >>= next
>     imp (Yield next) = next
> 

-- Part 3 State

We can define lists using the Free Monad. This uses ((,) a) as a functor that always stores one value in the first slot and the rest of the list in the second.

> type List' a = Free ((,) a) ()

But the monad behavior of this and the normal list monad are different. The list monad's join flattens nested lists whereas this concatenates lists.

-- Part 4 More Interpreters

We have a program type of [Response] -> [Request] that represents a pure pogram that reacts to outside responses by issuing requests to the outside. This is in the context of a game where players supply their own programs.

> data Direction = Up | Down | Left | Right deriving (Show)
> type Image = String
> data Interaction next =
>   Look Direction (Image -> next)
>   | Fire Direction next
>   | ReadLine (String -> next)
>   | WriteLine String (Bool -> next)


The interactions capture the relationsship between requests and responses. The constructor arguments that don't contain `next` (like Direction, String) are user-supplied and belong to the requests and the functions that have next as return type are the reponses to these requests. The interpreter pattern matches on the constructors so it has access to these functions (like `String -> next`) and can supply them with the correct arguments. If the interpreter is using the IO monad for example it can use getLine to get a String of its own, or otherwise it could supply one of a static set of strings it has access to.

We can define a functor instance for Interaction.

> instance Functor Interaction where
>   fmap f (Look dir g) = Look dir $ f . g
>   fmap f (Fire dir g) = Fire dir $ f g
>   fmap f (ReadLine g) = ReadLine $ f . g
>   fmap f (WriteLine s g) = WriteLine s $ f . g
>
> type Program = Free Interaction
> 
> look :: Direction -> Program Image
> look dir = liftF (Look dir id)
> 
> fire :: Direction -> Program ()
> fire dir = liftF (Fire dir ())
> 
> readLine :: Program String
> readLine = liftF (ReadLine id)
> 
> writeLine :: String -> Program Bool
> writeLine s = liftF (WriteLine s id)

Write an interpreter for it

> interpret :: Program a -> IO a
> interpret prg = foldFree pur imp prg
>   where
>     pur :: a -> IO a
>     pur = return
>     imp :: Interaction (IO next) -> IO next
>     imp (Look dir res) = putStrLn ("Player looks " ++ show dir) >> res ("You see " ++ show dir)
>     imp (Fire dir res) = putStrLn ("Player fires " ++ show dir) >> res
>     imp (ReadLine res) = putStrLn "Player reads" >> getLine >>= res
>     imp (WriteLine s res) = putStrLn ("Player writes " ++ show s) >> putStrLn s >> res True
>
> easyToAnger :: Program a
> easyToAnger = forever $ do
>   str <- readLine
>   when (str == "No") $ do
>     fire Up
>     _ <- writeLine "Take that!"
>     return ()

We can conclude that Free monads "free the interpreter". The programs that one can write using the Free Monad can use the nice do notation of monads, but all they do is build up a data structure without meaning. The interpreter can then fold over this datastructure to give it meaning.

--- Extra

What if we have a language that branches. So far all the languages have only had one next step, so the trees have actually been lists.

> data ExpAlg a = PrintExp String a | Plus a a deriving (Functor, Show)
> type Exp = Free ExpAlg
>
> printExp :: String -> Exp ()
> printExp s = liftF $ PrintExp s ()
>
> plus :: a -> a -> Exp a
> plus a1 a2 = liftF $ Plus a1 a2
>
> interpretExp :: (Show a) => Exp a -> IO a
> interpretExp prg = foldFree pur imp $ preprocess prg
>   where
>     pur a = return a
>     imp :: (Show a) => ExpAlg (IO a) -> IO a
>     imp (PrintExp s next) = print s >> next
>     imp (Plus next1 next2) = do
>       result1 <- next1
>       result2 <- next2
>       --print result1
>       --print result2
>       return result1

For preprocessing I would need to add a Pure around every Plus I think. So that I transform the program into a tree where each leaf is a Plus so that I can execute the path between two Pluses in one go and before I branch at the second Plus I can take the other path too.

>     preprocess :: Exp a -> Exp a
>     preprocess = id
>--     preprocess (Impure (Plus m1 m2)) = do
>--       next <- (Pure (Plus (preprocess m1) (preprocess m2)))
>--       liftF next

>       
> 
>
> branchingProgram :: Exp Int
> branchingProgram = do
>   b <- plus 2 5
>   c <- plus (b * 3) (b * 5)
>   printExp (show c)
>   return c

This results in a kind of branching program. Each Plus has two different next steps which are made by mapping the rest of the do block over its two arguments. So in one case b is 2 and in the other it's 5. The order in which they are evaluated depends on the interpreter. Here I first get the result from next1 and then next2. This is a nice example of how each value in the Pure is replaced by a new computation that uses that value, as plus creates a tree with two leaves and the function with \b -> rest-of-do-block gets applied to each leaf, creating a new tree that gets grafted on.


Further up I wondered whether a branching program with forever would diverge and yes, it does.
Intuitively because forever applies the program to itself, always replacing the leaves with new versions of the same program so we have an infinite tree.
a.d. Right now this only prints `True`. Can I write a different interpreter that walks the tree in a breadth-first way? Maybe if I do something similar to the interleave and preprocess the program so that I regain control at every plus by putting the rest of the monad after the plus into a Pure.

> divergingProgram :: Exp Bool
> divergingProgram = forever $ do
>   b <- plus True False
>   printExp (show b)
>   return b

