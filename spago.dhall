{ name = "linear-algebra"
, dependencies = [ "arrays", "foldable-traversable", "maybe", "partial", "prelude"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
