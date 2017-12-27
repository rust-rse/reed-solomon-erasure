{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

#if !MIN_VERSION_base(4, 8, 0)
import Data.Monoid (mempty)
#endif
import Data.Word (Word8)
import Text.Printf (printf)

import System.Random (Random(random), RandomGen, getStdGen)

import Options.Applicative
#if MIN_VERSION_optparse_applicative(0, 13, 0)
import Data.Monoid ((<>))
#endif

import System.Clock (Clock(Monotonic), TimeSpec(sec, nsec), getTime, diffTimeSpec)

import Control.DeepSeq (force)

import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import qualified Data.ReedSolomon as RS

data Options = Options { optionsN :: Int
                       , optionsK :: Int
                       , optionsSize :: Int
                       , optionsIterations :: Int
                       }
  deriving (Show, Eq)

parser :: Parser Options
parser =  Options
      <$> option auto
            ( short 'n'
           <> metavar "N"
           <> value 9
           <> showDefault
           <> help "Number of data shards"
            )
      <*> option auto
            ( short 'k'
           <> metavar "K"
           <> value 3
           <> showDefault
           <> help "Number of parity shards to calculate"
            )
      <*> option auto
            ( short 's'
           <> metavar "BYTES"
           <> value (1024 * 1024)
           <> showDefault
           <> help "Total data size to encode"
            )
      <*> option auto
            ( short 'i'
           <> metavar "COUNT"
           <> value 500
           <> showDefault
           <> help "Number of encoding iterations"
            )

go :: RS.Encoder -> V.Vector (SV.Vector Word8) -> Int -> IO ()
go enc shards = loop
  where
    loop n | n == 0 = return ()
           | otherwise = do
                parities <- force `fmap` RS.encode RS.defaultBackend enc shards
                parities `seq` loop (n - 1)

makeVector :: (SV.Storable a, Random a, RandomGen g) => g -> Int -> SV.Vector a
makeVector gen0 cnt = SV.unfoldrN cnt (Just . random) gen0

time :: IO () -> IO TimeSpec
time act = do
    start <- getTime Monotonic
    act
    diffTimeSpec start `fmap` getTime Monotonic

main :: IO ()
main = do
    Options{..} <- execParser $ info (helper <*> parser) mempty

    printf "Settings: N=%d K=%d size=%d iterations=%d\n"
        optionsN optionsK optionsSize optionsIterations

    enc <- RS.new optionsN optionsK
    vecs <- RS.split enc =<< flip makeVector optionsSize `fmap` getStdGen

    diff <- time (go enc vecs optionsIterations)

    printf "Total time: %ds %dns\n" (sec diff) (nsec diff)
