{-# LANGUAGE DeriveFunctor #-}
module RecursionSchemes where

-- this is our normal recursive datatype with which we can form expressions and evaluate those expressions
data ExprR = ConstR Int
  | AddR ExprR ExprR
  | MulR ExprR ExprR

evalR :: ExprR -> Int
evalR (ConstR x) = x
evalR (AddR e1 e2) = (evalR e1) + (evalR e2)
evalR (MulR e1 e2) = (evalR e1) * (evalR e2)


-- now we transform it into a functor without recursion
data ExprF a = ConstF Int
  | AddF a a
  | MulF a a
  deriving (Functor)

-- this is our fixpoint type, it uses recursion so that Fix of f is a potentially infinite tree representing an expression of f. It's a fixpoint because we have an isomorphism between Fix f ~ f (Fix f)
newtype Fix f = In (f (Fix f))

type Expr = Fix ExprF

-- an algebra is just something that can evaluate a functor
-- the nomenclature is a bit ambiguous here. An F-Algebra is actually a functor `f` together with a carrier type `a` and a function of this type.
type Algebra f a = f a -> a

-- an algebra to evaluate our expressions carrying Ints
evalF :: Algebra ExprF Int
evalF (ConstF x) = x
evalF (AddF x1 x2) = x1 + x2
evalF (MulF x1 x2) = x1 * x2

-- an algebra to evaluate the expressions as strings to pretty print them
ppF :: Algebra ExprF String
ppF (ConstF x) = show x
ppF (AddF s1 s2) = "(" ++ s1 ++ " + " ++ s2 ++ ")"
ppF (MulF s1 s2) = "(" ++ s1 ++ " * " ++ s2 ++ ")"

-- now we can use a catamorphism to apply these algebras to the fixpoint of the algebra's functor
cata :: Functor f => (Algebra f a) -> Fix f -> a
cata alg (In f) = alg (fmap (cata alg) f)

eval :: Expr -> Int
eval = cata evalF

pp :: Expr -> String
pp = cata ppF

-- they are pretty cumbersome to build. I think that's why people sometimes use free monads instead
-- (2 + 5) * (3 + 6)
test :: Expr
test = In (MulF (In (AddF (In (ConstF 2)) (In (ConstF 5))))
                 (In (AddF (In (ConstF 3)) (In (ConstF 6)))))

x1 = eval test -- 63
x2 = pp test -- "((2 + 5) * (3 + 6))"
