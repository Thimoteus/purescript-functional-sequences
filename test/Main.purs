module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Sequence.Functional (Sequence, (!!))
import Data.Sequence.Functional as Sequence

x :: Sequence Int
x = Sequence.fromFoldable [0,1,2,3,4,5,6,7,8,9]

y :: Sequence Int
y = Sequence.fromFoldable [0,1,2]

z :: Sequence Int
z = do
  a <- x
  b <- y
  pure (a*b)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (show z)
