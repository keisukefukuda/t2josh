module Tsubame.Options (
  Flag(..),
  parseOptions,
  ) where

import System.Console.GetOpt

data Flag = OptVersion
          | OptSelect String   -- done
          | OptWalltime String -- done
          | OptQueue String    -- done
          | OptGroup String    -- done
          | OptInclude String
          | OptPrompt
          | OptJoin String     -- done
          | OptMailEv String   -- done
          | OptMailTo String   -- done
          | OptStderr String   -- done
          | OptStdout String   -- done
          | OptJobName String  -- done
          | OptPriori String   -- done
          | OptRerunnable String -- done
          | OptHelp
          | OptJobIntp String -- done
          | OptVars String -- done
          | OptVarCopy -- done
          | OptAttrs String -- done
          | OptQuiet
          | OptVerify -- done
          | OptDryrun
          deriving(Show)

options :: [OptDescr Flag]
options = [ Option ['s'] ["select"]   (ReqArg OptSelect "SELECT")    "Number of nodes/procs/threads/gpus. See help."
          , Option [] ["walltime"]    (ReqArg OptWalltime "WTIME")    "-l walltime=hh:mm:ss"
          , Option ['q'] ["queue"]    (ReqArg OptQueue "QUEUE")       "Queue name"
          , Option ['g'] ["group"]    (ReqArg OptGroup "GROUP")       "Tsubame group name"
          , Option ['I'] ["include"]  (ReqArg OptInclude "INCLUDE")   "include a shell script (See --help)"
          , Option ['i'] []           (NoArg  OptPrompt)              "Prompt before submitting a job"
          , Option ['j'] ["join"]     (ReqArg OptJoin  "JOIN")        "Whether to combine the stdout/stderr files(oe/eo/m)"
          , Option ['m'] []           (ReqArg OptMailEv "MAIL_EVENTS") "Email on events 'n' or [a][b][c]"
          , Option ['M'] []           (ReqArg OptMailTo "MAILTO")     "Specify an email address"
          , Option ['e'] []           (ReqArg OptStderr "STDERR_FILE") "Standard error output file"
          , Option ['o'] []           (ReqArg OptStdout "STDOUT_FILE") "Standard output file"
          , Option ['N'] ["job-name"] (ReqArg OptJobName "JOB_NAME")  "Job name"
          , Option ['p'] []           (ReqArg OptPriori "PRIORITY")   "Job priority (0|1|2)"
          , Option ['r'] []           (ReqArg OptRerunnable "RERUN")   "A job is rerunnable"
          , Option ['h','?'] ["help"] (NoArg OptHelp)                 "Show help message"
          , Option ['S'] []           (ReqArg OptJobIntp "PATHS")     "Script interpreter"
          , Option ['v'] []           (ReqArg OptVars "VARS")         "Environment variables (var1=A[,var2=B,...])"
          , Option ['V'] []           (NoArg OptVarCopy)              "Copies user's login environment variables"
          , Option ['W'] []           (ReqArg OptAttrs "ATTRS")       "Additional job attributes (attr1=value1[,attr2=value2,...])"
          , Option ['z'] []           (NoArg OptQuiet)                "Do not print the job identifier"
          , Option [] ["verify"]      (NoArg OptVerify)               "Only verififes options and do not submit a job."
          , Option [] ["version"]     (NoArg OptVersion)              "Show version number"
          , Option [] ["dry"]         (NoArg OptDryrun)               "Just print the generated sciprt and do not run. Implies --verify."
          ]

parseOptions :: [String] -> ([Flag], [String], [String])
parseOptions = getOpt RequireOrder options
