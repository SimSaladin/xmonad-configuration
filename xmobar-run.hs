module Main where

import           MyXmobar
import           System.Environment (getArgs)

main :: IO ()
main = do
  [screen, sr, dpi, rds] <- getArgs
  xmobarRunMain (read screen) (read sr) (read dpi) (read rds)
