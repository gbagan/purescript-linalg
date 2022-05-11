{ name = "linear-algebra"
, dependencies = [ "arrays", "foldable-traversable", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
