module Proof where

import qualified Data.Map as M
import GHC.Generics

import qualified Term as T

data Hyp = Cite T.ID | Assume Int
  deriving (Show, Read)

data Jst = Tm T.Term | Pf Proof
  deriving (Show, Read)

data Proof
  = Intros [String] Proof
  | Apply Hyp [Jst]
  | Admit
  deriving (Show, Read)