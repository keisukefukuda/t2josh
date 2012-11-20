module Tsubame.T2Sub (
  Flag(..),
  buildScr,
  buildCmd,
  ) where

import Data.List
import Control.Monad.Writer
import Control.Monad.Identity

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

buildScr :: [Flag] -> [String] -> [String] -> Writer [String] String
buildScr flags args groups = return "Script!!"

strJoin :: String -> [String] -> String
strJoin sep xs =
  foldl (\s x -> if (length s) == 0 then x else s ++ sep ++ x) "" xs

-- Construct t2sub command line
buildCmd :: [Flag] -> [String] -> [String] -> Writer [String] String
buildCmd flags args groups = do
  let (cmds, msgs) = runWriter $ buildCmd' flags args groups
  WriterT $ Identity (strJoin " " cmds, msgs)

buildCmd' :: [Flag] -> [String] -> [String] -> Writer [String] [String]
buildCmd' flags args groups = do
  group <- chooseGroup flags groups
  return (["t2sub"] ++ group)

-- Generate "-g" option from flags
chooseGroup :: [Flag] -> [String] -> Writer [String] [String]
chooseGroup [] groups = WriterT $ Identity ([], ["TSUBAME group is not specified."])
chooseGroup flags groups =
  let opts = map (\x -> case x of OptGroup s -> s; _ -> "") flags
      grps = filter (\x -> length x > 0) opts
  in
   case length grps of
     0 -> WriterT $ Identity ([], ["No TSUBAME group is specified."])
     1 -> WriterT $ Identity (["-W", "group_list=" ++ (grps !! 0)],
                              if (grps !! 0) `elem` groups
                              then [""]
                              else ["You do not belong to TSUBAME group " ++ (grps !! 0)])
     _ -> error "More than one TSUBAME group is specified."

   {-
chooseWalltime :: [Flag] -> Writer [String] [String]
chooseWalltime flags =
  -}

--chooseAttr :: [Flag] -> Writer [String] [String]
