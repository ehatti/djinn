axiom "Specification of Hilbert's choice operator"
  Forall \p. Implies (Exists \x. p x) (p (HilbertChoice p))
end