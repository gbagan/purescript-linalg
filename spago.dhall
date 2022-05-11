{ name = "linear-algebra"
, dependencies = [ "arrays", "foldable-traversable", "maybe", "prelude"]
, testDependencies = ["console", "effect", "spec"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
