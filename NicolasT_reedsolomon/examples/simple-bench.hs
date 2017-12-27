{-# LANGUAGE CPP #-}
module Main (main) where

import Prelude hiding (sum)

#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)
import Data.Foldable (sum)
import Data.Maybe (fromJust)

import System.Environment (getArgs, getProgName)
import System.Random (getStdGen, randoms)

import Text.Printf (printf)

import qualified Data.Vector.Generic as V

import qualified Criterion as C
import qualified Criterion.Types as C
import qualified Statistics.Resampling.Bootstrap as S

import qualified Data.ReedSolomon as RS

runBench :: RS.Encoder -> RS.Matrix -> IO Double
runBench encoder shards = do
    let bench = C.nf (fromJust . RS.encode RS.defaultBackend encoder) shards

    r <- C.benchmark' bench

    let mean = S.estPoint $ C.anMean $ C.reportAnalysis r
        iters = 1.0 / mean
        dataSize = sum $ V.map V.length shards
        throughput = iters * fromIntegral dataSize

    return throughput

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 3) $ do
        progName <- getProgName
        error $ "Usage: " ++ progName ++ " dataShards parityShards dataSize"

    let [dataShards, parityShards, dataSize] = fmap read args

    instructionSet <- RS.simdInstructions
    printf "Using CPU instructions: %s\n" (maybe "None" show instructionSet)

    putStrLn "generating encoder..."
    encoder <- RS.new dataShards parityShards

    putStrLn "generating data..."
    vs <- RS.split encoder =<< V.fromListN dataSize . randoms <$> getStdGen

    throughput <- runBench encoder vs

    printf "Configuration: dataShards %d, parityShards %d, dataSize %d, shardSize %d\n"
        dataShards parityShards dataSize (V.length $ V.head vs)
    printf "Mean throughput: %.2f MB/s\n" (throughput / (1024 * 1024))
