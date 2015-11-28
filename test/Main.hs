module Main where

import qualified PactSpec as P
import qualified ServiceSpec as S

main :: IO ()
main = P.main >> S.main >> return ()