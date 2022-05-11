{ name = "linear-algebra"
, dependencies = [ "arrays", "foldable-traversable", "maybe", "prelude", "debug"]
, testDependencies = ["console", "effect"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
