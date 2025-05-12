module Xml where

import Data.List

data Elem = Elem String [(String, String)] [Elem] | Text String

render :: Elem -> String
render e = go "" e where
  go idn (Elem name attrs []) =
    idn ++ "<" ++ name ++ goAttrs attrs ++ "/>"
  go idn (Elem name attrs [Text s]) =
    idn ++ "<" ++ name ++ goAttrs attrs ++ ">" ++ s ++ "</" ++ name ++ ">"
  go idn (Elem name attrs subs) =
    idn ++ "<" ++ name ++ goAttrs attrs ++ ">\n" ++
    intercalate "\n" (map (go (idn ++ "  ")) subs) ++ "\n" ++
    idn ++ "</" ++ name ++ ">"
  go idn (Text s) =
    idn ++ s
  
  goAttrs attrs = concat (map (\(n, v) -> " " ++ n ++ "=\"" ++ v ++ "\"") attrs)