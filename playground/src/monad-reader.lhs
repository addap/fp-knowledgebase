> import Control.Monad.Reader

Our terms are lambda expressions with a parameter represented by a string and the body which is another Term, variables which are just strings and application of one term to another.

> data Term = Lambda String Term | Var String | Apply Term Term deriving (Show, Eq)
> newtype Env = Env [(String,Value)] deriving (Show, Eq)
> type Closure = (Term,Env)

An evaluated lambda still has the string parameter but a closure consisting of its body term _and_ the current environment from when the lambda was evaluated

> data Value = Lam String Closure | Failure String deriving (Show, Eq)


> interp' :: Term -> Reader Env Value

When we have a lambda term we can just evaluate it

> interp' (Lambda v t) = do
>   env <- ask
>   return $ Lam v (t, env)

When we have a variable we try to look it up in the environment and then evaluate the term of its closure in the environment of the closure.

> interp' (Var s) = do
>   (Env env) <- ask
>   case lookup s env of
>     Nothing -> return $ Failure $ "Unbound variable " ++ s
>     Just val -> return val

When we apply something we first interpret the first term and then update the environment to include the additional binding of the parameter.
The code from StackOverflow was wrong here, in the non-Failure case it interpreted t2 with the new environment which does not make sense, and also updated the environment instead of throwing it away and using the one from the closue (that would be dynamic scope I guess).

> interp' (Apply t1 t2) = do
>   v1 <- interp' t1
>   v2 <- interp' t2
>   case v1 of
>     Failure s -> return $ Failure s
>     Lam v (term, (Env env)) -> local (const (Env ((v,v2) : env))) $ interp' term
> interp :: Term -> Value
> interp t = runReader (interp' t) $ Env []

As defined in "Types and Progrmaming Languages" by Pierce

> tru = Lambda "t" $ Lambda "f" $ Var "t"
> fls = Lambda "t" $ Lambda "f" $ Var "f"
> conj = Lambda "b" $ Lambda "c" $ Apply (Apply (Var "b") (Var "c")) fls

True /\ True == True
True /\ False == False

> x1 = interp (Apply (Apply conj tru) tru) == interp tru
> x2 = interp (Apply (Apply conj tru) fls) == interp fls
