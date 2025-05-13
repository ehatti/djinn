theorem "Rewriting"
  Forall \x. Forall \y. Implies (Eq x y) (Forall \p. Implies (p x) (p y))
by
  intro (x y) [eq prf]
  admit
end