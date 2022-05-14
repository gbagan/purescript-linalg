{ name = "linear-algebra"
, dependencies =
  [ "arrays", "foldable-traversable", "maybe", "partial", "prelude", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
