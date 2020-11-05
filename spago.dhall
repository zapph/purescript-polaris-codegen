{ name = "polaris-codegen"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foreign-object"
  , "generics-rep"
  , "node-fs-aff"
  , "parsing"
  , "ps-cst"
  , "simple-json"
  , "string-extra"
  , "psci-support"
  , "st"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
