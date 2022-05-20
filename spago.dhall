{ name = "linalg"
, license = "MIT"
, repository = "https://github.com/gbagan/purescript-linalg"
, dependencies =
  [ "arrays"
  , "foldable-traversable"
  , "functions"
  , "maybe"
  , "prelude"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
