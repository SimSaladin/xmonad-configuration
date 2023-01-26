module Main where

import MyXmobar
import System.Environment (getArgs)

main :: IO ()
main = do
  [screen, sr, rds] <- getArgs
  xmobarRunMain (read screen) (read sr) (read rds)
