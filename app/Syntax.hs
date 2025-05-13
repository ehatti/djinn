module Syntax where

type ID = String

data Term
  = Var Int
  | Lam String Term
  | App Term Term
  | Con ID
  deriving Show

shift :: Term -> Term
shift = go 0 where
  go c e = case e of
    Var i ->
      if i < c then
        Var i
      else
        Var (i + 1)
    Lam x e -> Lam x (go (1 + c) e)
    App f v -> App (go c f) (go c v)
    Con f -> Con f

subst :: Term -> Term -> Term
subst = go 0 where
  go c e s = case e of
    Var i ->
      if i < c then
        Var i
      else if i == c then
        s
      else
        Var (i - 1)
    Lam x e -> Lam x (go (1 + c) e (shift s))
    App f v -> App (go c f s) (go c v s)
    Con f -> Con f

norm :: Term -> Term
norm e = case e of
  Var i -> Var i
  Lam x e -> Lam x (norm e)
  App f v ->
    let v' = norm v in
    case norm f of
      Lam x e -> subst e v'
      f' -> App f' v'
  Con f -> Con f

equal :: Term -> Term -> Bool
equal a b = case (a, b) of
  (Var i, Var j) -> i == j
  (Lam _ a, Lam _ b) -> equal a b
  (App f v, App g w) -> equal f g && equal v w
  (Con f, Con g) -> f == g
  _ -> False

data Source
  = Assume Int
  | Cite ID
  deriving Show

data Proof
  = Apply Source [Term] [Proof]
  | Intro [String] [String] Proof
  | Admit
  deriving Show

data Block
  = Prose String
  | Include ID
  deriving Show

data Object
  = Section String [Block]
  | Definition String Term
  | Theorem String Term Proof
  | Constant String
  | Axiom String Term
  deriving Show

index :: Eq a => a -> [a] -> Maybe Int
index _ [] = Nothing
index x (y:ys) | x == y = pure 0
index x (_:ys) = (1+) <$> index x ys