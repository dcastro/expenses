module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Discovery (discoverAndRunSpecs)
import Test.Spec.Reporter (consoleReporter)

main :: Effect Unit
main = discoverAndRunSpecs [ consoleReporter ] """..*Spec"""
