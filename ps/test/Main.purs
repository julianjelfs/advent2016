module Test.Main where

import Prelude
import Day1 (answer)
import Test.Assert (assert)

main = do
  assert (answer == 271)
