module Tsubame.Job (
  Queue(..),
  Job(..),
  queueName,
  parseJobs
  ) where

import Data.List
import Text.Regex
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

parseJobs :: String -> [Job]
parseJobs strs = filterNothing $ map parseJob $ lines strs

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
