module Main
  ( main
  ) where

import           Hi.Cli             (run)
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= run
