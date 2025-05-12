module Compile where

import Syntax

import Data.Map qualified as M

type Error = String

type Info = Maybe (Term, Maybe Error)

type Env = M.Map ID Term
data Ctx = Ctx
  { bindings :: [String]
  , assumes :: [(String, Term)] }

bind :: Ctx -> String -> Ctx
bind (Ctx bs hs) b = Ctx (b:bs) hs

assume :: Ctx -> String -> Term -> Ctx
assume (Ctx bs hs) h a = Ctx bs ((h, a) : hs)

pattern Implies a b = App (App (Con "Implies") a) b
pattern Forall x b = App (Con "Forall") (Lam x b)

cast :: Proof () -> Proof Info
cast prf = case prf of
  Admit () -> Admit Nothing
  Intro () bs hs subprf -> Intro Nothing bs hs (cast subprf)
  Apply () src es subprfs -> Apply Nothing src es (map cast subprfs)

intro :: Env -> Ctx -> [String] -> [String] -> Term -> Proof () -> Either String (Proof Info)
intro env ctx bs hs goal subprf = case (bs, hs, goal) of
  ([], [], goal) -> Right (check env ctx subprf goal)
  (b:bs, _, Forall _ e) -> intro env (bind ctx b) bs hs e subprf
  (_, h:hs, Implies a e) -> intro env (assume ctx h a) bs hs e subprf
  _ -> Left "Too many intros"

apply :: Env -> Ctx -> [Term] -> [Proof ()] -> Term -> Either String ([Proof Info], Term)
apply env ctx es js goal = case (es, js) of
  ([], []) -> Right ([], goal)
  (e:es, _) -> case goal of
    Forall x subgoal -> apply env ctx es js (subst subgoal e)
    _ -> Left "Too many specializations"
  (_, j:js) -> case goal of
    Implies a subgoal -> do
      (js', result) <- apply env ctx es js subgoal
      Right (check env ctx j a : js', result)
    _ -> Left "Too many justifications"

find :: Env -> Ctx -> Source -> Maybe Term
find env ctx src = case src of
  Cite ref -> M.lookup ref env
  Assume i ->
    if i >= 0 && i < length (assumes ctx) then
      Just (snd (assumes ctx !! i))
    else
      Nothing

check :: Env -> Ctx -> Proof () -> Term -> Proof Info
check env ctx prf goal = case prf of
  Admit () -> Admit (Just (goal, Nothing))
  Intro () bs hs subprf ->
    case intro env ctx bs hs goal subprf of
      Right subprf' -> Intro (Just (goal, Nothing)) bs hs subprf'
      Left err -> Intro (Just (goal, Just err)) bs hs (cast subprf)
  Apply () src es js ->
    case find env ctx src of
      Just hyp -> case apply env ctx es js hyp of
        Right (js', result) ->
          if equal (norm goal) (norm result) then
            Apply (Just (goal, Nothing)) src es js'
          else
            Apply (Just (goal, Just "Result does not match goal")) src es js'
        Left err -> Apply (Just (goal, Just err)) src es (map cast js)
      Nothing -> Apply (Just (goal, Just "Application head not found")) src es (map cast js)