module Tsubame.T2Sub (
  Flag(..),
  parseRequest,
  ) where

import Data.List
import Control.Monad.Writer

data Flag = OptVersion
          | OptNumProc String
          | OptQueue String
          | OptGroup String
          | OptInclude String
          | OptPrompt
          | OptJoin String
          | OptMailEv String
          | OptMailTo String
          | OptStderr String
          | OptStdout String
          | OptJobName String
          | OptPriori String
          | OptRerunnable
          | OptHelp
          | OptJobIntp String
          | OptVars String
          | OptVarCopy
          | OptAttrs String
          | OptQuiet
          | OptVerify
          | OptDryrun
          deriving(Show)

data Compiler   = Intel | PGI | GNU
data MPILibrary = OpenMPI | MVAPICH2 | MVAPICH1

parseRequest :: [Flag] -> [String] -> [String] -> (String, String)
parseRequest flags args groups = (genScript flags args groups, buildCmd flags args groups)

genScript :: [Flag] -> [String] -> [String] -> String
genScript flags args groups = "Script!!"

strJoin :: String -> [String] -> String
strJoin sep xs =
  foldl (\s x -> if (length s) == 0 then x else s ++ sep ++ x) "" xs

flagGroup :: [Flag] -> [String] -> [String] -> String
flagGroup flags args groups = groups !! 0

-- Construct t2sub command line
buildCmd :: [Flag] -> [String] -> [String] -> String
buildCmd flags args groups =
  strJoin " " $ ["t2sub"] ++ (chooseGroup flags groups)

-- Generate "-g" option from flags
chooseGroup :: [Flag] -> [String] -> [String]
chooseGroup [] groups = []
chooseGroup (x:xs) groups =
  case x of
    OptGroup g -> if g `elem` groups
                  then ["-g", g]
                  else error ("You do not belong to a group \"" ++ g ++ "\"")
    _          -> chooseGroup xs groups
  
