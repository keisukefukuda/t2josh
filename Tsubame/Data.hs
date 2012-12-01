module Tsubame.Data (
  Compiler,
  MPILib(..)
) where

data Compiler   = Intel | PGI | GNU
                deriving (Show)
data MPILib = OpenMPI | MVAPICH2 | UnknownMPI
            deriving (Show)

