module Display where

import Syntax
import Compile
import Xml

import Data.Maybe(fromJust)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.List(intercalate)

import Debug.Trace

import Control.Applicative

data Move = Down | None | Reset

type Notation = Term -> Maybe ([(Move, [String], Term)], [String] -> String)

findNotation :: [Notation] -> Notation
findNotation [] _ = Nothing
findNotation (n:ns) e = n e <|> findNotation ns e

displayTerm :: [[Notation]] -> Term -> String
displayTerm all e = go 0 [] e where
  go :: Int -> [String] -> Term -> String
  go ix bs e = case ix == length all of
    True -> case e of
      Var i -> bs !! i
      Con f -> "\\mathtt{"  ++ f ++ "}"
      _ -> "\\left(" ++ go 0 bs e ++ "\\right)"
    False -> case findNotation (all !! ix) e of
      Just (subterms, render) ->
        render
          (map
            (\(m, sbs, se) ->
              case m of
                Down -> go (ix + 1) (reverse sbs ++ bs) se
                None -> go ix (reverse sbs ++ bs) se
                Reset -> go 0 (reverse sbs ++ bs) se)
            subterms)
      Nothing ->
        go (ix + 1) bs e

collectApps :: Term -> [Term]
collectApps (App f v) = collectApps f ++ [v]
collectApps f = [f]

initNs :: S.Set ID -> [[Notation]]
initNs nd =
  [ [ (\e -> case e of
        Forall x e ->
          Just ([(Reset, [x], e)], \[s] -> "\\forall " ++ x ++ ". \\," ++ s)
        _ -> Nothing)
    , (\e -> case e of
        App (Con "Exists") (Lam x e) ->
          Just ([(Reset, [x], e)], \[s] -> "\\exists " ++ x ++ ". \\," ++ s)
        _ -> Nothing)
    , (\e -> case e of
        Lam x e -> Just ([(Reset, [x], e)], (\[s] -> "\\lambda " ++ x ++ ". \\," ++ s))
        _ -> Nothing) ]
  , [ (\e -> case e of
        Implies a b ->
          Just ([(Down, [], a), (Reset, [], b)], \[a, b] -> a ++ " \\Rightarrow " ++ b)
        _ -> Nothing) ]
  , [ (\e -> case e of
        App (App (Con "Eq") a) b ->
          Just ([(Down, [], a), (Down, [], b)], \[a, b] -> a ++ " = " ++ b)
        _ -> Nothing) ]
  , [ (\e -> case e of
        App (Con "HilbertChoice") p ->
          Just ([(Down, [], p)], \[a] -> "\\epsilon \\, " ++ a)
        _ -> Nothing)
    , (\e -> case e of
        App f v ->
          let es = collectApps (App f v) in
          case es of
            Con f : _ | S.member f nd -> Nothing
            _ -> Just (map (\e -> (Down, [], e)) es, \ss -> intercalate " \\, " ss)
        _ -> Nothing) ]
  , [] ]

initNd :: S.Set ID
initNd = S.fromList ["Forall", "Implies", "Eq", "Exists", "HilbertChoice"]

strip :: String -> String
strip s = case s of
  c:s | elem c [' ', '\n'] -> strip s
  s -> s

formatProse s = reverse (strip (reverse (strip s)))

displayBlock :: M.Map ID (Object Info) -> Block -> Elem
displayBlock objs b = case b of
  Include name -> displayObject objs name (objs M.! name)
  Prose p -> Elem "prose" [] [ Text (formatProse p) ]

termXML :: Term -> Elem
termXML e = case e of
  Var i -> Elem "var" [] [Text (show i)]
  Lam x e -> Elem "lam" [("name", x)] [termXML e]
  App f v -> Elem "app" [] [termXML f, termXML v]
  Con f -> Elem "con" [] [Text f]

displayObject :: M.Map ID (Object Info) -> ID -> Object Info -> Elem
displayObject objs name obj = case obj of
  Definition t e ->
    Elem "definition" []
      [ Elem "title" [] [ Text t ]
      , Elem "term" []
          [ termXML e ]
      , Elem "display" []
          [ Text (displayTerm (initNs initNd) e) ]
      ]
  Theorem t a p ->
    Elem "theorem" []
      [ Elem "title" [] [ Text t ]
      , Elem "goal" []
        [ Elem "term" []
          [ termXML a ]
        , Elem "display" []
          [ Text (displayTerm (initNs initNd) a) ]
        ]
      ]
  Constant t ->
    Elem "constant" []
      [ Elem "title" [] [ Text t ]
      ]
  Axiom t a ->
    Elem "axiom" []
      [ Elem "title" [] [ Text t ]
      , Elem "goal" []
        [ Elem "term" []
          [ termXML a ]
        , Elem "display" []
          [ Text (displayTerm (initNs initNd) a) ]
        ]
      ]
  Section t bs ->
    Elem "section" []
      ( Elem "title" [] [ Text t ]
      : map (\b -> Elem "block" [] [ displayBlock objs b ]) bs )