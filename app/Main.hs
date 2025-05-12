module Main where

import Syntax
import Parse
import Compile
import Xml
import Display

import Text.Megaparsec

import System.Exit
import System.Directory
import System.FilePath

import Data.Set qualified as S
import Data.Map qualified as M

import Control.Monad

readObject :: String -> IO (Object ())
readObject file = do
  raw <- readFile file
  case parse (object <* eof) file raw of
    Right obj -> pure obj
    Left err -> do
      putStrLn $ errorBundlePretty err
      exitFailure

readDir :: String -> IO (M.Map ID (Object ()))
readDir dir = do
  files <- listDirectory dir
  list <- (zip (map (fst . splitExtension) files)) <$> traverse readObject (map ((dir ++ "/") ++) files)
  pure (M.fromList list)

dependencies :: Object i -> S.Set ID
dependencies obj = case obj of
  Section _ bs -> mconcat (map goBlock bs)
  Definition _ e -> goTerm e
  Theorem _ e p -> mconcat [goTerm e, goProof p]
  Constant _ -> mempty
  Axiom _ e -> goTerm e
 where
  goBlock :: Block -> S.Set ID
  goBlock b = case b of
    Prose _ -> mempty
    Include name -> S.singleton name
  goTerm :: Term -> S.Set ID
  goTerm e = case e of
    Var _ -> mempty
    Lam _ e -> goTerm e
    App f v -> mconcat [goTerm f, goTerm v]
    Con f -> S.singleton f
  goProof :: Proof i -> S.Set ID
  goProof p = case p of
    Apply _ src es ps ->
      let des = mconcat (map goTerm es) in
      let dps = mconcat (map goProof ps) in
      let dsrc =
            case src of
              Cite f -> S.singleton f
              _ -> mempty in
      mconcat [dsrc, des, dps]
    Intro _ _ _ subprf -> goProof subprf
    Admit _ -> mempty

getThms :: [(ID, Object Info)] -> [(ID, Term)]
getThms objs = case objs of
  [] -> []
  (name, Theorem _ a _) : objs -> (name, a) : getThms objs
  _ : objs -> getThms objs

elabObject :: S.Set ID -> M.Map ID (Object ()) -> Object () -> IO (Object Info)
elabObject used objs this = do
  let deps = dependencies this
  loaded <- forM (S.toList deps) \name ->
    if S.member name used then do
      putStrLn $ "Cyclic dependency detected: " ++ name
      exitFailure
    else case M.lookup name objs of
      Just obj -> do
        obj <- elabObject (S.insert name used) objs obj
        pure (name, obj)
      Nothing -> do
        putStrLn $ "Object not found: " ++ name
        exitFailure
  let env = M.fromList (getThms loaded)
  case this of
    Section t bs -> pure (Section t bs)
    Definition t e -> pure (Definition t e)
    Theorem t a p -> pure (Theorem t a (check env (Ctx [] []) p a))
    Constant t -> pure (Constant t)
    Axiom t a -> pure (Axiom t a)

elabDir :: String -> IO [(ID, Object Info)]
elabDir dir = do
  raws <- readDir dir
  let raws' = M.insert "Forall" (Constant "Universal quantification") (M.insert "Implies" (Constant "Logical implication") raws)
  forM (M.toList raws') \(name, obj) -> do
    obj' <- elabObject mempty raws' obj
    pure (name, obj')

objectTaxon :: Object i -> String
objectTaxon obj = case obj of
  Definition _ _ -> "Definition"
  Theorem _ _ _ -> "Theorem"
  Constant _ -> "Constant"
  Axiom _ _ -> "Axiom"
  Section _ _ -> "Section"

objectTitle :: Object i -> String
objectTitle obj = case obj of
  Definition t _ -> t
  Theorem t _ _ -> t
  Constant t -> t
  Axiom t _ -> t
  Section t _ -> t

displayRefs :: M.Map ID (Object i) -> S.Set ID -> [Elem]
displayRefs objs deps =
  map
    (\d ->
      let obj = objs M.! d in
      Elem "reference"
        [ ("addr", d)
        , ("taxon", objectTaxon obj)
        , ("title", objectTitle obj) ]
        [ ])
    (S.toList deps)

dependents :: ID -> [(ID, Object i)] -> S.Set ID
dependents name objs = case objs of
  [] -> mempty
  (othr, obj) : objs ->
    let deps = dependents name objs in
    if S.member name (dependencies obj) then
      S.insert othr deps
    else
      deps

compileDir :: String -> String -> IO ()
compileDir inDir outDir = do
  objs <- elabDir inDir
  forM_ objs \(name, obj) -> do
    let xml = displayObject (M.fromList objs) name obj
    writeFile (outDir ++ "/" ++ name ++ ".xml")
      (render
        (Elem "document" []
          [ xml
          , Elem "dependencies" []
            (displayRefs (M.fromList objs) (dependencies obj))
          , Elem "dependents" []
            (displayRefs (M.fromList objs) (dependents name objs))]))

main = pure ()