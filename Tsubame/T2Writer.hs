module T2Writer (
  T2Writer
) where

import Data.Monoid
import Control.Applicative

data T2Writer a = T2Writer { contents :: Maybe a
                           , warns    :: [String]
                           , errors   :: [String]
                           }
                deriving (Show)

instance Monad T2Writer where
  return x = T2Writer (Just x) [] []

  (T2Writer (Just x) w e) >>= f = let (T2Writer x' w' e') = f x
                                  in T2Writer x' (w ++ w') (e ++ e')
  (T2Writer Nothing w e)  >>= f = T2Writer Nothing w e

