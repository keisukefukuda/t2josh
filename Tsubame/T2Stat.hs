module Tsubame.T2Stat (
  t2stat, t2statAll,
  Compiler, MPILibrary,
  JobQuery
  ) where

import System.Process
import Data.Time.Clock
import Control.Monad

import Tsubame.Job

data Compiler   = Intel | PGI | GNU
data MPILibrary = OpenMPI | MVAPICH2 | MVAPICH1

data JobQuery = JobQuery [String]  -- commands
                         Queue     -- queue
                         (Maybe Int) -- openmp (serial if Nothing)
                         (Maybe Int) -- mpi    (serial if Nothing)
                         (Maybe Int) -- CUDA   (CUDA version)
              deriving(Show)

-- Get queue name from a Job value

t2statAll :: IO ([Job])
t2statAll = do
  src <- readProcess "t2stat" ["-all", "-w"] ""
  return $ parseJobs src
                               
t2stat :: IO ([Job])
t2stat = do
  src <- readProcess "t2stat" [] ""
  return $ parseJobs src
