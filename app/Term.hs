module Term where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import Data.Void
import GHC.Generics

newtype ID = ID String
  deriving (Eq, Ord, Generic, Show, Read)

data Term
  = Var Int
  | Lam String Term
  | App Term Term
  | Con ID
  deriving (Generic, Show, Read)

pattern Forall x e = App (Con (ID "forall")) (Lam x e)
pattern Implies a b = App (App (Con (ID "implies")) a) b

shift :: Term -> Term
shift = go 0 where
  go c e = case e of
    Var i ->
      if i < c then
        Var i
      else
        Var (1 + i)
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
      Lam x e -> norm (subst e v')
      f' -> App f' v'
  Con f -> Con f

equal :: Term -> Term -> Bool
equal e t = case (e, t) of
  (Var i, Var j) | i == j -> True
  (Lam _ e, Lam _ t) -> equal e t
  (App f v, App g w) -> equal f g && equal v w
  (Con f, Con g) | f == g -> True
  _ -> False