module Tsubame.T2Sub (
  Flag(..),
  buildScr,
  buildCmd,
  ) where

import Data.List
import Data.Char
import Control.Monad.Writer
import Control.Monad.Identity
import Tsubame.Utils

data Flag = OptVersion
          | OptNumProc String
          | OptMemory String
          | OptWalltime String -- done
          | OptQueue String    -- done
          | OptGroup String    -- done
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
  attrs <- chooseAttr flags
  queue <- chooseQueue flags
  wtime <- chooseWalltime flags
  join  <- chooseJoin flags
  return (["t2sub"] ++ queue ++ group ++ attrs ++ wtime ++ join)

-- Generate "-g" option from flags
chooseGroup :: [Flag] -> [String] -> Writer [String] [String]
chooseGroup [] groups = WriterT $ Identity ([], ["TSUBAME group is not specified."])
chooseGroup flags groups =
  let grps = concatMap (\x -> case x of OptGroup s -> [s]; _ -> []) flags
  in
   case length grps of
     0 -> WriterT $ Identity ([], ["No TSUBAME group is specified."])
     1 -> WriterT $ Identity (["-W", "group_list=" ++ (grps !! 0)],
                              if (grps !! 0) `elem` groups
                              then []
                              else ["You do not belong to TSUBAME group " ++ (grps !! 0)])
     _ -> error "More than one TSUBAME group is specified."

chooseAttr :: [Flag] -> Writer [String] [String]
chooseAttr flags =
  let attrs = concatMap (\x -> case x of OptAttrs s -> [s]; _ -> []) flags
  in
   return $ concat $ map (\x -> ["-W", x]) attrs

chooseQueue :: [Flag] -> Writer [String] [String]
chooseQueue flags =
  let queue = concatMap (\x -> case x of OptQueue s -> [s]; _ -> []) flags
      msgs = case length queue of
        0 -> ["No queue name is specified. Using S instead."]
        1 -> []
        _ -> ["More than 1 queue is specified. Using the first one."]
      queue2 = queue ++ ["S"]
  in
   WriterT $ Identity (["-q", queueName (queue2 !! 0)], msgs)
  where
    queueName :: String -> String
    queueName q =
      let q' = map toUpper q
      in if q' == "X" then "S"
         else if q' `elem` ["S", "S96", "L128", "L256", "L512", "G", "V", "VW", "SW", "H"]
              then q
              else error $ "Error: Unknown queue name : " ++ q

chooseWalltime :: [Flag] -> Writer [String] [String]
chooseWalltime flags =
  let wtimes = concatMap (\x -> case x of OptWalltime s -> [s]; _ -> []) flags
  in case length wtimes of
    0 -> return []
    1 -> return ["-l", "walltime=" ++ (wtimes !! 0)]
    _ -> WriterT $ Identity (["-l", "walltime=" ++ (last $ sortBy compWtime wtimes)],
                             ["Multiple walltime is specified. Using the longest one."])

chooseJoin :: [Flag] -> Writer [String] [String]
chooseJoin flags =
  let joins = concatMap (\x -> case x of OptJoin s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> ["-j"] ++ [x]) joins
