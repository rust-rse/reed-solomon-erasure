module Main (main) where

import Data.Word

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.Process as P (shell)
import System.Process.ByteString ()
import qualified System.Process.Common as P

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM

type Runner = Word8x16 -> Word8x16 -> Data -> IO (ExitCode, ByteString, ByteString)

newtype Word8x16 = Word8x16 [Word8]
  deriving (Show, Eq)

instance QC.Arbitrary Word8x16 where
    arbitrary = Word8x16 `fmap` QC.vector 16

newtype Data = Data [Word8]
  deriving (Show, Eq)

instance QC.Arbitrary Data where
    arbitrary = do
        cnt <- QC.arbitrary
        -- When not using Positive, cnt happens to be 0 quite often,
        -- and testing empty input is not very valuable
        Data `fmap` QC.vector (QC.getPositive cnt * 32)

prop_equal :: Runner
           -> Runner
           -> Word8x16
           -> Word8x16
           -> Data
           -> QC.Property
prop_equal runner1 runner2 low high dat = QCM.monadicIO $ do
    result1 <- QCM.run (runner1 low high dat)
    result2 <- QCM.run (runner2 low high dat)

    let (exitCode1, _, _) = result1

    QCM.assert (exitCode1 == ExitSuccess)
    QCM.assert (result1 == result2)

runner :: String -> Runner
runner cmd (Word8x16 low) (Word8x16 high) (Data dat) = do
    let stdin = BS.concat [BS.pack low, BS.pack high, BS.pack dat]
        len = show $ length dat
        proc = P.shell $ unwords [cmd, len]
    P.readCreateProcess proc stdin

main :: IO ()
main = do
    args <- getArgs
    (verbose, cmd1, cmd2) <- case args of
                                 ["-v", cmd1, cmd2] -> return (True, cmd1, cmd2)
                                 [cmd1, cmd2] -> return (False, cmd1, cmd2)
                                 _ -> usage >> exitFailure
    let runner1 = runner cmd1
        runner2 = runner cmd2
        check = if verbose then QC.verboseCheck else QC.quickCheck
    check $ prop_equal runner1 runner2
  where
    usage = do
        prog <- getProgName
        hPutStrLn stderr $ "Usage: " ++ prog ++ " [-v] cmd1 cmd2"
