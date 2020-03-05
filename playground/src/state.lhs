> import Control.Monad.State
> import Data.Tuple.Extra

runState is  a function from a State and its initial state to the final result and state. It unrolls the state computation that is represented by State

> x1 = runState (return 'X') 1

For example `get` is a State that just sets the result to the state. So the following returns (1,1)

> x2 = runState get 1

And put is a function that takes a value and sets the state to that value and the result to (). So the following return ((), 5)

> x3 = runState (put 5) 1

State is a monad so we can use do notation

> x4 = runState (do { put 5; return 'X' }) 1
> x5 = runState (put 5 >> return 'X') 1

modify takes a function and applies it to the state, gets takes a function and applies it to the result after a get

> x6 = runState (modify (+1)) 1
> x7 = runState (gets (+1)) 1

evalState and execState evaluate to just the result and the state respectively

> x8 = evalState (gets (+1)) 1
> x9 = execState (gets (+1)) 1

This game build on the state monad. The playGame function build a State value that describes the entire computation from the startState to the final state.

Ex. playGame "ab" results in a get, put, get put, get, return chain.

> type GameValue = Int
> type GameState = (Bool, Int)

The first case of playGame puts the final state in the correct form by only taking the second element of the state as the final result

> playGame :: String -> State GameState GameValue
> playGame []     = gets snd

The second case of playGame can just operate on the state. The do block makes a long chain of bound get's and put's.

> playGame (x:xs) = do
>     (on, score) <- get
>     case x of

can also use modify here instead of put by combining the functions to work on pairs of their inputs using (***) from Date.Tuple.Extra

>          'a' | on -> modify $ id *** (+1)
>          'b' | on -> put (on, score - 1)
>          'c'      -> put (not on, score)
>          _        -> put (on, score)
>     playGame xs
> 
> startState = (False, 0)
> 
> main = print $ evalState (playGame "abcaaacbbcabbab") startState


Example of the StateT transformer to mix the IO and State Monad
We have a Monad instance of IO.
Therefore we have a MonadIO instance MonadIO (StateT Int IO)
and a MonadState instance MonadState Int (StateT Int IO)
so here m = StateT Int IO
so the return type of tick will be StateT Int IO () which is a StateT that has an internal Int state, produces a () i.e. uninteresting result and can use the IO features like printing stuff.

> tick :: (Show s, Num s, MonadIO m, MonadState s m) => m ()
> tick = do
>   x <- get
>   liftIO $ putStrLn ("incrementing " ++ (show x))
>   put (x+1)
> 
> x10 = (runStateT (tick >> tick >> tick >> tick) 5) :: IO ((), Int)

