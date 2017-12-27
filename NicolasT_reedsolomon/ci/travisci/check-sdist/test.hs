module Main (main) where

import Data.ReedSolomon (simdInstructions)
import Data.ReedSolomon.BuildInfo (buildInfo)

main :: IO ()
main = do
    print buildInfo
    print =<< simdInstructions
