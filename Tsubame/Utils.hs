module Tsubame.Utils (
  wtime2sec,
  compWtime
) where

import qualified Data.Text as T
import Debug.Trace

wtime2sec :: String -> Int
wtime2sec str =
  let hms = map T.unpack $ T.split (==':') $ T.pack str
  in if length hms /= 3
     then error $ "Invalid walltime format(1) : '" ++ str ++ "'"
     else
       let h = reads (hms !! 0) :: [(Int,String)]
           m = reads (hms !! 1) :: [(Int,String)]
           s = reads (hms !! 2) :: [(Int,String)]
       in if length h == 0 || length m == 0 || length s == 0
          then error $ "Invalid walltime format(2) : '" ++ str ++ "'"
          else
            let hi = fst (h !! 0)
                mi = fst (m !! 0)
                si = fst (s !! 0)
            in hi * 3600 + mi * 60 + si

compWtime :: String -> String -> Ordering
compWtime s1 s2 = compare (wtime2sec s1) (wtime2sec s2)

