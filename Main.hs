{-

Options:
  -h, --help            show this help message and exit
  --version             
  -n NP, --num-proc=NP
      ex.) 8,    8 MPI process (8 nodes)
           4x2,  4 nodes, 2 process/node, 8 processes in total
           6x2x2 6 nodes, 2 MPI processes/node, 2 OpenMP threads
  -q QUEUE, --queue=QUEUE
                        Queue to use
  -g GROUP, --group=GROUP
                        Your TSUBAME2 group
  -m MEMORY, --memory=MEMORY
                        Memory per chunk
  -N JOBNAME, --jobname=JOBNAME
                        Job name (same as t2sub's -N)
  --openmp=OMP          Number of OpenMP threads
  --cuda-version=CUDA_VERSION
                        CUDA Toolkit version(3.0/3.1/3.2/4.0/4.1)
  --mpi-env=MPI_ENV     MPI environment (ex. Intel+OpenMPI, usually
                        automatically selected from the current environment)
  --mpi                 Whether to use MPI (usually automatically configured)
  --place=PLACE         --place option to t2sub (usually automatically configured)
  --no-mpi              Don't use MPI
  --gpus=GPUS           Number of GPUs per chunk(=node)
  -p PASSTHROUGH, --pass-through=PASSTHROUGH
                        Arguments passed through to t2sub command
  -d DIR, --dir=DIR     Working directory (default: current directory
  --home                Equivalent to --dir=$HOME
  -Q, --quiet           Execute t2sub command silently
  -i, --prompt          
  --dry                 Just print and do nothing (dry run)
  -v, --verify          Use t2sub's '-verify' option
  -W WARNING
-}

{-
  ***  Option Parsing  ***
-}

module Main where

import System.Console.GetOpt
import System.Environment

import Tsubame.Job
import Tsubame.T2Stat

data Flag = OptVersion
          | OptDryrun
          | OptNumProc String
            deriving(Show)
            
options :: [OptDescr Flag]
options = [ Option ['n'] ["num-proc"] (ReqArg OptNumProc "NUM_PROC") "Number of processes/threads"
          , Option ['V'] ["version"] (NoArg OptVersion) "Show version number"
          , Option []    ["dry"]     (NoArg OptDryrun)  "Just print and do nothing (dry run)"
          ]

main :: IO()
main = do
  args <- getArgs
  let (flags, nonOpts, msgs) = getOpt RequireOrder options args
  putStrLn $ show $ length flags
  putStrLn $ show flags
  putStrLn $ show nonOpts

  jobs <- t2statAll
  -- mapM_ (putStrLn . show . jobDuration) jobs
  return ()

