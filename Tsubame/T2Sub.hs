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
import Tsubame.Select
import Tsubame.Data
import Tsubame.Options

buildScr :: [Flag] -> [String] -> [String] -> Writer [String] [String]
buildScr flags args groups = do
  tell [""]
  return [""]

-- Construct t2sub command line
-- buildCmd flags args groups
buildCmd :: [Flag] -> [String] -> [String] -> Writer [String] String
buildCmd flags args groups = do
  let (cmds, msgs) = runWriter $ buildCmd' flags args groups
  tell msgs
  return $ strJoin " " cmds

buildCmd' :: [Flag] -> [String] -> [String] -> Writer [String] [String]
buildCmd' flags args groups = do
  group   <- chooseGroup flags groups
  attrs   <- chooseAttr flags
  queue   <- chooseQueue flags
  wtime   <- chooseWalltime flags
  join    <- chooseJoin flags
  sel     <- chooseSelect flags
  mailev  <- chooseMailEv flags
  mailto  <- chooseMailTo flags
  stdout  <- chooseStdout flags
  stderr  <- chooseStderr flags
  priori  <- choosePriori flags
  jobname <- chooseJobName flags
  rerun   <- chooseRerun flags
  intp    <- chooseIntp flags
  vars    <- chooseVars flags
  varcopy <- chooseVarCopy flags
  attrs   <- chooseAttr flags
  verify  <- chooseVerify flags
  return (["t2sub"] ++ sel ++ queue ++ group ++ attrs ++ wtime ++ join ++
          mailev ++ mailto ++ stderr ++ stdout ++ priori ++ jobname ++ rerun ++
          intp ++ vars ++ varcopy ++ attrs ++ verify)

-- Generate "-g" option from flags
chooseGroup :: [Flag] -> [String] -> Writer [String] [String]
chooseGroup [] groups = tell ["TSUBAME group is not specified."] >> return []
chooseGroup flags groups =
  let grps = concatMap (\x -> case x of OptGroup s -> [s]; _ -> []) flags
  in
   case length grps of
     0 -> tell ["No TSUBAME group is specified."] >> return []
     1 -> do
       let g = grps !! 0
       when (not $ (g `elem` groups)) $ do
         tell ["You do not belong to TSUBAME group " ++ g]
       return ["-W", "group_list=" ++ g]
     _ -> error "More than one TSUBAME group is specified."

chooseAttr :: [Flag] -> Writer [String] [String]
chooseAttr flags =
  let attrs = concatMap (\x -> case x of OptAttrs s -> [s]; _ -> []) flags
  in
   return $ concat $ map (\x -> ["-W", x]) attrs

chooseQueue :: [Flag] -> Writer [String] [String]
chooseQueue flags =
  let queues = concatMap (\x -> case x of OptQueue s -> [s]; _ -> []) flags
  in if length queues == 0
     then tell ["No queue is specified. Using 'S'."] >> return ["-q", "S"]
     else return $ concat $ map (\x -> ["-q"] ++ [x]) queues

chooseWalltime :: [Flag] -> Writer [String] [String]
chooseWalltime flags =
  let wtimes = concatMap (\x -> case x of OptWalltime s -> [s]; _ -> []) flags
  in case length wtimes of
    0 -> return []
    1 -> return ["-l", "walltime=" ++ (wtimes !! 0)]
    _ -> do
      tell ["Multiple walltime is specified. Using the longest one."]
      return ["-l", "walltime=" ++ (last $ sortBy compWtime wtimes)]

chooseJoin :: [Flag] -> Writer [String] [String]
chooseJoin flags =
  let joins = concatMap (\x -> case x of OptJoin s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> ["-j"] ++ [x]) joins

chooseSelect :: [Flag] -> Writer [String] [String]
chooseSelect flags =
  let errTooMany = "Error: Multiple -s options are specified"
      sels = concatMap (\x -> case x of OptSelect s -> [s]; _ -> []) flags
  in return $ buildSelect $ parseSelect $ case compare (length sels) 1 of
                                            LT -> ""
                                            GT -> error errTooMany
                                            EQ -> sels !! 0

chooseMailEv :: [Flag] -> Writer [String] [String]
chooseMailEv flags =
  let errUnknown = "Error: Unknown parameter for -m"
      evs = concatMap (\x -> case x of OptMailEv s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> if x `elem` ["n", "a", "b", "e"]
                                  then ["-m", x]
                                  else error $ errUnknown) evs

chooseMailTo :: [Flag] -> Writer [String] [String]
chooseMailTo flags =
  let mailto = concatMap (\x -> case x of OptMailTo s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> ["-m", x]) mailto

chooseStderr :: [Flag] -> Writer [String] [String]
chooseStderr flags =
  let stderr = concatMap (\x -> case x of OptStderr s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> ["-e", x]) stderr

chooseStdout :: [Flag] -> Writer [String] [String]
chooseStdout flags =
  let stdout = concatMap (\x -> case x of OptStdout s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> ["-o", x]) stdout

chooseJobName :: [Flag] -> Writer [String] [String]
chooseJobName flags =
  let stdout = concatMap (\x -> case x of OptJobName s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> ["-N", x]) stdout

choosePriori :: [Flag] -> Writer [String] [String]
choosePriori flags =
  let stdout = concatMap (\x -> case x of OptPriori s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> ["-p", x]) stdout

chooseRerun :: [Flag] -> Writer [String] [String]
chooseRerun flags =
  let stdout = concatMap (\x -> case x of OptRerunnable s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> ["-r", x]) stdout

-- check the existance of job interpreter ?
chooseIntp :: [Flag] -> Writer [String] [String]
chooseIntp flags =
  let stdout = concatMap (\x -> case x of OptJobIntp s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> ["-S", x]) stdout

chooseVars :: [Flag] -> Writer [String] [String]
chooseVars flags =
  let vars = concatMap (\x -> case x of OptVars s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> ["-v", x]) vars

chooseVarCopy :: [Flag] -> Writer [String] [String]
chooseVarCopy flags =
  return $ concatMap (\x -> case x of OptVarCopy -> ["-V"]; _ -> []) flags

chooseAttrs :: [Flag] -> Writer [String] [String]
chooseAttrs flags =
  let attrs = concatMap (\x -> case x of OptAttrs s -> [s]; _ -> []) flags
  in return $ concat $ map (\x -> ["-W", x]) attrs

chooseVerify :: [Flag] -> Writer [String] [String]
chooseVerify flags =
  return $ concatMap (\x -> case x of OptVarCopy -> ["-verify"]; _ -> []) flags

