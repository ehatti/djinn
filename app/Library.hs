module Library where

import qualified Data.Map as M
import GHC.Generics
import Data.List(intercalate)

import qualified Term as T
import qualified Proof as P
import Xml
import Debug.Trace

import Control.Monad
import Data.IORef
import System.Process

data Object
  = Definition String T.Term
  | Theorem String T.Term P.Proof
  deriving (Show, Read)

type Theorems = M.Map T.ID T.Term
type Context = [(String, T.Term)]
type Bindings = [String]

collectApp :: T.Term -> [T.Term]
collectApp (T.App f v) = collectApp f ++ [v]
collectApp f = [f]

collectLam :: T.Term -> ([String], T.Term)
collectLam (T.Lam x e) =
  let (xs, e') = collectLam e in
  (x:xs, e')
collectLam e = ([], e)

termXML :: Bindings -> T.Term -> Elem
termXML bs e = case e of
  T.Var i -> Elem "variable" [("name", bs !! i)] [Text (show i)]
  T.Lam x e ->
    let (xs, e') = collectLam (T.Lam x e) in
    Elem "function" []
      [ Elem "bindings" [] (map (\x -> Elem "binding" [("name", x)] []) xs)
      , Elem "body" [] [termXML (reverse xs ++ bs) e']
      ]
  T.App f v ->
    let e:es = collectApp (T.App f v) in
    Elem "application" []
      ( Elem "head" [] [termXML bs e]
      : map (\e -> Elem "argument" [] [termXML bs e]) es
      )
  T.Con (T.ID f) -> Elem "reference" [] [Text f]

getIntros :: [String] -> T.Term -> Maybe (Bindings, Context, [(String, Maybe T.Term)], T.Term)
getIntros ns e = case (ns, e) of
  ([], goal) -> pure ([], [], [], goal)
  (n : ns, T.Forall _ subgoal) -> do
    (bs, hs, ints, subgoal') <- getIntros ns subgoal
    pure (n : bs, hs, (n, Nothing) : ints, subgoal')
  (n : ns, T.Implies hyp res) -> do
    (bs, hs, ints, subgoal') <- getIntros ns res
    pure  (bs, (n, hyp) : hs, (n, Just hyp) : ints, subgoal')
  _ -> error $ show (ns, e)

introsXML :: Bindings -> [(String, Maybe T.Term)] -> [Elem]
introsXML bs ints = case ints of
  [] -> []
  (n, int) : ints -> case int of
    Just hyp -> Elem "hypothesis" [("name", n)] [termXML bs hyp] : introsXML bs ints
    Nothing -> Elem "binding" [("name", n)] [] : introsXML (n : bs) ints

getHyp :: Theorems -> Context -> P.Hyp -> Either String (T.Term, Elem)
getHyp thms asms hyp = case hyp of
  P.Assume i ->
    if i >= 0 && i < length asms then
      let (name, prop) = asms !! i in
      Right (prop, Elem "assume" [("name", name)] [Text (show i)])
    else
      Left ("Out-of-bounds DB index " ++ show i)
  P.Cite (T.ID name) -> case M.lookup (T.ID name) thms of
    Just prop -> Right (prop, Elem "cite" [] [Text name])
    Nothing -> Left ("No such theorem " ++ name)

apply :: Theorems -> Bindings -> Context -> T.Term -> [P.Jst] -> ([Elem], T.Term)
apply thms bs asms prop jsts = case jsts of
  [] -> ([], prop)
  P.Tm e : jsts -> case prop of
    T.Forall x prop ->
      let (elems, res) = apply thms bs asms (T.norm (T.subst prop e)) jsts in
      let elem = Elem "specialize" [("name", x)] [termXML bs e] in
      (elem : elems, res)
  P.Pf prf : jsts -> case prop of
    T.Implies a b ->
      let (elems, res) = apply thms bs asms b jsts in
      let elem = Elem "justify" [] [Elem "goal" [] [termXML bs a], Elem "proof" [] [check thms bs asms a prf]] in
      (elem : elems, res)

check :: Theorems -> Bindings -> Context -> T.Term -> P.Proof -> Elem
check thms bs asms goal prf = case prf of
  P.Intros ns subprf -> case getIntros ns goal of
    Just (binds, hyps, ints, subgoal) ->
      Elem "introduce" []
        [ Elem "suppose" [] (introsXML bs ints)
        , Elem "goal" [] [termXML (reverse binds ++ bs) subgoal]
        , Elem "proof" [] [check thms (reverse binds ++ bs) (reverse hyps ++ asms) subgoal subprf]
        ]
    Nothing ->
      Elem "error" [] [Text "Too many variables to introduce"]
  P.Apply hyp jsts ->
    case getHyp thms asms hyp of
      Right (prop, propXML) ->
        let (elems, given) = apply thms bs asms prop jsts in
        let given' = T.norm given in
        let goal' = T.norm goal in
        if T.equal given' goal' then
          Elem "apply" [] ([propXML] ++ elems)
        else
          let err = Elem "error" [] [Text "Result of application does not match goal", termXML bs given', termXML bs goal'] in
          Elem "apply" [] ([propXML] ++ elems ++ [err])
      Left err ->
        Elem "error" [] [Text err]
  P.Admit ->
    Elem "admit" []
      [ Elem "context" [] (map (\(x, h) -> Elem "hypothesis" [("name", x)] [termXML bs h]) (reverse asms))
      , Elem "goal" [] [termXML bs goal]
      ]

unId :: T.ID -> String
unId (T.ID n) = n

objectXML :: T.ID -> Theorems -> Object -> (Elem, T.Term)
objectXML name thms obj = case obj of
  Theorem title stmt prf ->
    ( Elem "theorem" []
        [ Elem "title" [] [Text title]
        , Elem "ident" [] [Text (unId name)]
        , Elem "goal" [] [termXML [] stmt]
        , Elem "proof" [] [check thms [] [] stmt prf]
        ]
    , stmt
    )
  Definition title def ->
    ( Elem "definition" []
        [ Elem "title" [] [Text title]
        , Elem "ident" [] [Text (unId name)]
        , Elem "value" [] [termXML [] def]
        ]
    , T.Forall "P" (T.Implies (T.App (T.Var 0) (T.shift def)) (T.App (T.Var 0) (T.Con name)))
    )

writeLib :: FilePath -> Theorems -> [(T.ID, Object)] -> IO ()
writeLib dir thms objs = case objs of
  [] -> pure ()
  (T.ID name, obj) : objs -> do
    let (el, ax) = objectXML (T.ID name) thms obj
    writeFile (dir ++ "/" ++ name ++ ".xml") (render el)
    writeLib dir (M.insert (T.ID name) ax thms) objs
    callCommand ("xsltproc " ++ dir ++ "/style.xml " ++ dir ++ "/" ++ name ++ ".xml > " ++ dir ++ "/" ++ name ++ ".html")