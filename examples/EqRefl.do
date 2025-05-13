theorem "Reflexivity of equality"
  Forall \x. Eq x x
by
  intro (x) []
  apply Eq (\f. f x x)
  intro () [h]
  apply h
end