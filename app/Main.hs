module Main where

import Term
import Proof
import Library
import Xml
import Parse

appc f = foldl App (Con (ID f))
appv i = foldl App (Var i)

lib :: [(ID, Object)]
lib =
  [ ( ID "double-app"
    , Definition
        "Double application"
        (pterm "|f| |x| f x x")
    )
  , ( ID "conj"
    , Definition
        "Logical conjunction"
        (pterm "|P| |Q| :forall (|R| :implies (:implies P (:implies Q R)) R)")
    )
  , ( ID "conj-intro"
    , Theorem
        "Conjunction introduction"
        (pterm ":forall (|P| :forall (|Q| :implies P (:implies Q (:conj P Q))))")
        (Intros ["P", "Q", "hP", "hQ"]
          (Apply
            (Cite (ID "conj"))
            [ Tm (Lam "F" (appe (Var 0) [Var 2, Var 1]))
            , Pf
              (Intros ["R", "hI"]
                (Apply
                  (Assume 0)
                  [ Pf (Apply (Assume 2) [])
                  , Pf (Apply (Assume 1) [])
                  ]))
            ]))
    )
  , ( ID "eq"
    , Definition
        "Equality"
        (pterm "|x| |y| :forall (|P| :implies (P x) (P y))")
    )
  , ( ID "im-triv"
    , Theorem
      "The trivial implication"
      (pterm ":forall (|P| :implies P P)")
      (Intros ["P", "h"] (Apply (Assume 0) []))
    )
  , ( ID "eq-refl"
    , Theorem
        "Reflexivity of equality"
        (pterm ":forall (|x| :eq x x)")
        (Intros ["x"]
          (Apply
            (Cite (ID "eq"))
            [ Tm (Lam "F" (appe (Var 0) [Var 1, Var 1]))
            , Pf
              (Intros ["P"]
                (Apply (Cite (ID "im-triv"))
                  [Tm (App (Var 0) (Var 1))]))
            ]))
    )
  ]

main = writeLib "examples" mempty lib