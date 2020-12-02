module AOC(module Prelude, module AOC) where

import Prelude hiding(interact)
import qualified Prelude

interact f = Prelude.interact $ (++"\n") . show . f
