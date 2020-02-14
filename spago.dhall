{ name = "openBridge"
, dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "lists"
    , "maybe"
    , "psci-support"
    , "st"
    , "strings"
    , "stringutils"
    , "validation"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
