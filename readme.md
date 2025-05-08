# Djinn

Tools like [Forester](https://www.jonmsterling.com/index/index.xml), TeX, or Typst are _markup languages_ for writing documents. Djinn is an attempt at combining markup languages with interactive theorem provers -- the formal text may be machine-checked, and also compiled down to a human-readable form.

This is a super-prototype so currently the definitions and theorems to check-and-compile are just hardcoded into `Main.hs`, but we can do some interesting things like defining equality, conjunction, and proving simple facts about them. Running the executable will check the hardcoded objects and compile them to HTML, placing them in the `examples` folder.