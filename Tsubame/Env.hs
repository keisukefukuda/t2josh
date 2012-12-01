module Tsubame.Env (
  getMPILib
) where

import Text.Regex
import System.Posix.Env
import System.Directory

import Tsubame.Data

getMPILib :: IO (MPILib)
getMPILib = do
  mpicc <- findExecutable "mpicc"
  case mpicc of
    Just p  -> case matchRegex (mkRegex "openmpi") p of
      Just _  -> return OpenMPI
      Nothing -> case matchRegex (mkRegex "mvapich2") p of
        Just _ -> return MVAPICH2
        Nothing -> return UnknownMPI


        

