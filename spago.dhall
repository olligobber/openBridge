{ name = "openBridge"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "halogen"
  , "lists"
  , "maybe"
  , "psci-support"
  , "st"
  , "strings"
  , "stringutils"
  , "test-unit"
  , "transformers"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
