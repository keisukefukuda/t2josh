module Tsubame where

import System.Environment
import System.Process
import System.Console.GetOpt
import Data.Time.Clock
import Data.List
import Text.Regex
import Control.Monad
import Debug.Trace

data Queue = S | S96 | X | H | L128 | L128F | L256 | L512 | V | G
           deriving (Show)

data Job = Job { jobID    :: String
               , jobName  :: String
               , jobOwner :: String
               , jobDuration :: Integer
               , jobState :: String
               , jobQueue :: Queue
               } deriving(Show)

data Compiler   = Intel | PGI | GNU
data MPILibrary = OpenMPI | MVAPICH2 | MVAPICH1

data JobQuery = JobQuery [String]  -- commands
                         Queue     -- queue
                         (Maybe Int) -- openmp (serial if Nothing)
                         (Maybe Int) -- mpi    (serial if Nothing)
                         (Maybe Int) -- CUDA   (CUDA version)
              deriving(Show)

-- TODO : Switch Compiler & MPI library

-- Get queue name from a Job value
queueName :: String -> String -> Queue
queueName id q =
  case q of
    "S"     -> if "05" `isSuffixOf` id
               then X
               else S
    "S96"   -> S96
    "H"     -> H
    "V"     -> V
    "G"     -> G
    "L128"  -> L128
    "L128F" -> L128
    "L256"  -> L256
    "L512"  -> L512
    _ -> if "R" `isPrefixOf` q
         then H
         else error ("Unkown Queue Name:" ++ q)

parseJob :: String -> Maybe(Job)
parseJob str =
  let cols = words str
  in if (length cols) /= 11 || ("----" `isPrefixOf` (cols !! 0))
     then Nothing
     else let d = parseDuration $ cols !! 10
          in case d of
               Nothing -> Nothing
               Just sec -> Just $ Job (cols !! 0) -- jobID
                                      (cols !! 3) -- jobName
                                      (cols !! 1) -- jobOwner
                                      sec         -- jobDuration
                                      (cols !! 9) -- jobState
                                      (queueName (cols !! 0) (cols !! 2)) -- jobQueue
  where
    parseDuration :: String -> Maybe Integer
    parseDuration str =
      if str == "0" || str == "--"
      then Just 0
      else let dur = splitRegex (mkRegex ":") str
           in if (length dur) /= 3
              then (trace ("parseDuration error : str = " ++ (show dur)) Nothing)
              else let h = read $ dur !! 0 :: Integer
                       m = read $ dur !! 1 :: Integer
                       s = read $ dur !! 2 :: Integer
                   in Just (60 * 60 * h + m * 60 + s)

filterNothing :: [Maybe Job] -> [Job]
filterNothing jobs =
  foldl (\ls x -> case x of Nothing -> ls; Just a -> ls ++ [a]) [] jobs

{-
  ***  Command Launcher  ***
-}
    
t2statAll :: IO ([Job])
t2statAll = do
  src <- readProcess "t2stat" ["-all", "-w"] ""
  return $ filterNothing $ map parseJob $ lines src
                               
t2stat :: IO ([Job])
t2stat = do
  src <- readProcess "t2stat" [] ""
  return $ filterNothing $ map parseJob $ lines src

{-
  ***  Option Parsing  ***
-}
    
data Flag = Version
options :: [OptDescr Flag]
options = [ Option ['V'] ["version"] (NoArg Version) "Show version number" ]

main :: IO()
main = do
  args <- getArgs
  let (flags, nonOpts, msgs) = getOpt RequireOrder options args

  jobs <- t2statAll
  mapM_ (putStrLn . show . jobDuration) jobs
  return ()
