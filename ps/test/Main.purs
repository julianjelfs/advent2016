module Test.Main where

import Prelude
import Day1 (processInstructions, input)
import Test.Assert (assert)

main = do
    assert (processInstructions input == 271)
