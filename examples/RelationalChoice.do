theorem "Axiom of choice"
  Forall \r. Implies (Forall \x. Exists \y. r x y) (Exists \f. Forall \x. r x (f x))
by
  admit
end