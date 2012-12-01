module Main where

import System.Environment
import System.Exit

import Control.Monad
import Control.Monad.Writer
import Data.List

import Tsubame.Options
import Tsubame.Utils
import Tsubame.Env
import Tsubame.T2Stat
import Tsubame.T2Group
import Tsubame.T2Sub

--TODO
-- Allow long job name

{-
*** How to specify --select option ***

Syntax of -s option

-s a:b:c

a : Number of node
b : Number of MPI processes per node
c : Number of OpenMP threads of each process

Examples:

** no specification
  A serial job. A single node is allocated.
  Equivalent to -l select=1:mpiprocs=1:ncpus=12:mem=52gb:gpus=3
  "mem=52gb" is specified sicne it is non-MPI job (refer to the manual).
  (Note that ncpus and mem are specified anyway)

** -s ::12
  An SMP job using OpenMP on a single node.
  Equivalent to "-l select=1:mpiprocs=1:ncpus=12:mem=52gb:gpus=3"
  In the job script, OMP_NUM_THREADS=12 is exported.
  "mem=52gb" is specified sicne it is non-MPI job (refer to the manual).

** -s 16:12
  Use 16 nodes, 12 MPI processes/node, 16*12=192 processes in total.
  Equivalent to: "-l select=16:mpiprocs=12:ncpus=12:mem=50gb:gpus=3 -l place=scatter"
  Note that mem=50gb is specified anyway, although 4*12=48.

** -s 16:1:12 or -s 16::12
  Run 16*1=16 MPI processes, each process runs 12 OpenMP/Pthread threads.
  1 MPI process/node is seleted because [b] is omitted.

  Equivalent to: "-l select=16:mpiprocs=1:ncpus=12:mem=50gb:gpus=3 -l place=scatter"

** -s 8:3:4
  Use 8 nodes, with 3 MPI processes/node, 8*3=24 MPI processes in total,
  and each process uses 3 OpenMP threads.

  Equivalent to:
    "-l select=8:mpiprocs=3:ncpus=12:mem=50gb:gpus=3 -l place=scatter"

** -s 32:1:12
Run 32*1 MPI processes, with 3 GPUs and 12 OpenMP threads for each process.

Equivalent to:
  -l select=32:mpiprocs=1:ncpus=12:gpus=3 -l place=scatter
-}

main :: IO()
main = do
  args <- getArgs
  let (flags, nonOpts, msgs) = parseOptions args

  when (length msgs > 0) $ do
    putStrLn "Error:  "
    mapM_ putStr msgs
    exitWith $ ExitFailure (-1)

  putStrLn $ show $ length flags
  putStrLn $ show flags
  putStrLn $ show nonOpts

  groups <- t2group
  mpi <- getMPILib
  putStrLn "Script content = "
  putStrLn "t2sub command line = "
  let (cmd, msg1) = runWriter $ buildCmd flags args groups
  let (scr, msg2) = runWriter $ buildScr flags args groups
  mapM_ (\x -> putStrLn $ "t2sub: Warning: " ++ x) msg1
  mapM_ (\x -> putStrLn $ "t2sub: Warning: " ++ x) msg2
  print $ cmd ++ " " ++ (strJoin " " nonOpts)

  return ()
