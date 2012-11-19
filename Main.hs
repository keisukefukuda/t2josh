module Main where

import System.Console.GetOpt
import System.Environment
import System.Exit

import Control.Monad

import Tsubame.T2Stat
import Tsubame.T2Group
import Tsubame.T2Sub

--TOOD
-- Allow long job name

{-
*** How to specify --select option ***

Syntax of -s option

a/b/c[,gpus=N][,mem=N]

a : Number of node
b : Number of MPI processes per node
c : Number of OpenMP threads of each process
gpus=N : Number of GPUs *per node*
mem=N  : Amount of memory *per process*

Examples:

** -s single
Single job. mpirun is not used.

** -s 1
MPI with 1 process. -l select=1

** -s 16/12,mem=4gb
Run 16*12=192 processes in total. Use 16 nodes and 12 MPI processes per node.
4gb*12 memory is used on each node.

Equivalent to:
  -l select=16:mpiprocs=12:mem=48gb -l place=scatter

** -s 16/1/12,mem=40gb
Run 16*1=16 MPI processes, each process runs 12 OpenMP/Pthread threads.

Equivalent to:
  -l select=16:mpiprocs=1:ncpus=12:mem=40gb -l place=scatter

** -s 8/3/4,gpus=1,mem=15gb
Run 8*3=24 MPI processes, with 1 GPU, 4 OpenMP threads, and 15gb memory for each process.

Equivalent to:
  -l select=8:mpiprocs=3:ncpus=12:mem=35gb:gpus=3 -l place=scatter

** -s 32/1/12,gpus=3
Run 32*1 MPI processes, with 3 GPUs and 12 OpenMP threads for each process.

Equivalent to:
  -l select=32:mpiprocs=1:ncpus=12:gpus=3 -l place=scatter

** -s //12,gpus=3
-}

options :: [OptDescr Flag]
options = [ Option ['s'] ["select"]   (ReqArg OptNumProc "SELECT")   "Number of nodes/processes/threads/gpus. See --help"
          , Option ['q'] ["queue"]    (ReqArg OptQueue "QUEUE")      "Queue name"
          , Option ['g'] ["group"]    (ReqArg OptGroup "GROUP")      "Tsubame group name"
          , Option ['I'] ["include"]  (ReqArg OptInclude "INCLUDE")  "include a shell script (See --help)"
          , Option ['i'] []           (NoArg  OptPrompt)   "Prompt before submitting a job"
          , Option ['j'] ["join"]     (ReqArg OptJoin  "JOIN")       "Whether to combine the stdout/stderr files(oe/eo/m)"
          , Option ['m'] []     (ReqArg OptMailEv "MAIL_EVENTS") "Email on events 'n' or [a][b][c]"
          , Option ['M'] []   (ReqArg OptMailTo "MAILTO")    "Specify an email address"
          , Option ['e'] []   (ReqArg OptStderr "STDERR_FILE") "Standard error output file"
          , Option ['o'] []   (ReqArg OptStdout "STDOUT_FILE") "Standard output file"
          , Option ['N'] ["job-name"] (ReqArg OptJobName "JOB_NAME") "Job name"
          , Option ['p'] []   (ReqArg OptPriori "PRIORITY") "Job priority (0|1|2)"
          , Option ['r'] [] (NoArg OptRerunnable) "A job is rerunnable"
          , Option []    ["version"]  (NoArg OptVersion) "Show version number"
          , Option ['h','?'] ["help"] (NoArg OptHelp)    "Show help message"
          , Option ['S'] [] (ReqArg OptJobIntp "PATHS")    "Script interpreter"
          , Option ['v'] [] (ReqArg OptVars "VARS") "Environment variables (var1=A[,var2=B,...])"
          , Option ['V'] [] (NoArg OptVarCopy) "Copies user's login environment variables"
          , Option ['W'] [] (ReqArg OptAttrs "ATTRS") "Additional job attributes (attr1=value1[,attr2=value2,...])"
          , Option ['z'] [] (NoArg OptQuiet)   "Do not print the job identifier"
          , Option [] ["verify"] (NoArg OptVerify) "Only verififes options and do not submit a job."
          , Option [] ["dry"]      (NoArg OptDryrun)  "Just print the generated sciprt and do not run. Implies --verify."
          ]

main :: IO()
main = do
  args <- getArgs

  let (flags, nonOpts, msgs) = getOpt RequireOrder options args

  when (length msgs > 0) $ do
    putStrLn "Error:  "
    mapM_ putStr msgs
    exitWith $ ExitFailure (-1)

  putStrLn $ show $ length flags
  putStrLn $ show flags
  putStrLn $ show nonOpts

  groups <- t2group
  let (script, command) = parseRequest flags nonOpts groups
  putStrLn "Script content = "
  putStrLn script
  putStrLn "t2sub command line = "
  putStrLn command

  -- jobs <- t2statAll
  -- mapM_ (putStrLn . show . jobDuration) jobs
  return ()

