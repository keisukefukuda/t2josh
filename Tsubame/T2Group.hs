module Tsubame.T2Group (
  t2group
  ) where

import System.Process

t2group :: IO([String])
t2group = do
  out <- readProcess "t2group" [] ""
  return $ parseOutput out

-- t2g-sample         used      |    1.0 used          0 |    1.0 used           8329832
parseOutput :: String -> [String]
parseOutput src =
  let ln = drop 3 $ lines src
  in map (head . words) ln

      