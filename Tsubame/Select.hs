module Tsubame.Select (
  buildSelect,
  parseSelect
) where

import Debug.Trace
import qualified Data.Text as T

import Tsubame.Data

buildSelect :: (Bool, Int, Int, Int) -> [String]
buildSelect (isMPI, numNodes, numMPI, numOmpTh) =
  if not isMPI
  then ["-l", "select=1:mpiprocs=1:ncpus=12:mem=52gb:gpus=3", "-l", "place=scatter"]
  else
    let nodes = show numNodes
        procs = show numMPI
    in ["-l", "select=" ++ nodes ++ ":mpiprocs=" ++ procs ++ ":ncpus=12:mem=50gb:gpus=3", "-l", "place=scatter"]

{--
  "(#nodes)[:(#mpiprocs)]" -> (isMPIjob, #nodes, #mpiprocs, #ompthreads)
  So far, the following values are fixed.
     #cpus = 12
     #gpus = 3
     #threads
     place = scatter
--}
parseSelect :: String -> (Bool, Int, Int, Int)
parseSelect "" = (False, 0, 0, 12)
parseSelect str =
  let s = split' ':' str   -- "12:2:6" -> ["12", "2", "6"]
      p = map readSelectArg s
  in case length s of
    1 -> (True, p !! 0, 1, 0) -- Only  #nodes is specified.
    2 -> if (s !! 0 == "" && s !! 1 /= "")
         then error ("Invalid select specification: " ++ str)
         else (True, p !! 0, p !! 1, 0) -- #nodes and #mpiprocs are specified
    3 -> let nodes = p !! 0
             mpi   = max (p !! 1) 1
             omp   = p !! 2
         in if nodes == 0
            then (False, 0, 0, 12)
            else (True, nodes, mpi, omp)
  where
    readSelectArg :: String -> Int
    readSelectArg "" = 0
    readSelectArg src =
      let rd = reads src :: [(Int, String)]
      in if null rd
         then error ("Error: Invalid value for -s option: '" ++ src ++ "'")
         else fst $ rd !! 0

split' :: Char -> String -> [String]
split' c s = map T.unpack $ T.split (== c) $ T.pack s

