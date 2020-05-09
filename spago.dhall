{ name = "polaris-codegen"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foreign-object"
  , "generics-rep"
  , "node-fs-aff"
  , "simple-json"
  , "psci-support"
  , "st"
  ]
, packages = ../../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
