> {-# LANGUAGE CPP #-}
> module Main (main) where
>
> import Control.Exception (catchJust, fromException)
> import Control.Monad (when)
> import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4, 8, 0)
> import Data.Monoid (mempty)
#endif
> import Text.Printf (printf)
> import System.IO (IOMode(WriteMode), withFile)
> import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
>
> import System.IO.Posix.MMap (unsafeMMapFile)
>
> import Options.Applicative
#if MIN_VERSION_optparse_applicative(0, 13, 0)
> import Data.Monoid ((<>))
#endif
>
> import qualified Data.ByteString as BS
>
> import qualified Data.Vector.Generic as V
>
> import Data.Vector.Storable.ByteString (fromByteString, toByteString)
>
> import qualified Data.ReedSolomon as RS

//+build ignore

// Copyright 2015, Klaus Post, see LICENSE for details.
//
// Simple decoder example.
//
// The decoder reverses the process of "simple-encoder.go"
//
// To build an executable use:
//
// go build simple-decoder.go
//
// Simple Encoder/Decoder Shortcomings:
// * If the file size of the input isn't diviable by the number of data shards
//   the output will contain extra zeroes
//
// * If the shard numbers isn't the same for the decoder as in the
//   encoder, invalid output will be generated.
//
// * If values have changed in a shard, it cannot be reconstructed.
//
// * If two shards have been swapped, reconstruction will always fail.
//   You need to supply the shards in the same order as they were given to you.
//
// The solution for this is to save a metadata file containing:
//
// * File size.
// * The number of data/parity shards.
// * HASH of each shard.
// * Order of the shards.
//
// If you save these properties, you should abe able to detect file corruption
// in a shard and be able to reconstruct your data if you have the needed number of shards left.

package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/klauspost/reedsolomon"
)

var dataShards = flag.Int("data", 4, "Number of shards to split the data into")
var parShards = flag.Int("par", 2, "Number of parity shards")
var outFile = flag.String("out", "", "Alternative output path/file")

> data Options = Options { optionsData :: Int
>                        , optionsPar :: Int
>                        , optionsOut :: Maybe FilePath
>                        , optionsFname :: FilePath
>                        }
>   deriving (Show, Eq)
>
> parser :: Parser Options
> parser =  Options
>       <$> option auto
>             ( long "data"
>            <> metavar "N"
>            <> value 4
>            <> showDefault
>            <> help "Number of shards to split the data into, must be below 257."
>             )
>       <*> option auto
>             ( long "par"
>            <> metavar "K"
>            <> value 2
>            <> showDefault
>            <> help "Number of parity shards"
>             )
>       <*> optional (strOption
>             ( long "out"
>            <> metavar "PATH"
>            <> help "Alternative output directory"
>             ))
>       <*> strArgument
>             ( metavar "FILE"
>            <> help "File to encode"
>             )

func init() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage of %s:\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "  simple-decoder [-flags] basefile.ext\nDo not add the number to the filename.\n")
		fmt.Fprintf(os.Stderr, "Valid flags:\n")
		flag.PrintDefaults()
	}
}

func main() {

> main :: IO ()
> main = do

	// Parse flags
	flag.Parse()

>     options <- execParser $ info (helper <*> parser) mempty

	args := flag.Args()
	if len(args) != 1 {
		fmt.Fprintf(os.Stderr, "Error: No filenames given\n")
		flag.Usage()
		os.Exit(1)
	}
	fname := args[0]

>     let fname = optionsFname options
>         dataShards = optionsData options
>         parShards = optionsPar options

	// Create matrix
	enc, err := reedsolomon.New(*dataShards, *parShards)
	checkErr(err)

>     enc <- RS.new dataShards parShards

	// Create shards and load the data.
	shards := make([][]byte, *dataShards+*parShards)
	for i := range shards {
		infn := fmt.Sprintf("%s.%d", fname, i)
		fmt.Println("Opening", infn)
		shards[i], err = ioutil.ReadFile(infn)
		if err != nil {
			fmt.Println("Error reading file", err)
			shards[i] = nil
		}
	}

>     shards <- V.generateM (dataShards + parShards) $ \i -> do
>         let infn = concat [fname, ".", show i]
>         printf "Opening %s\n" infn
>         catchJust
>             (\e ->
>                 if isDoesNotExistErrorType (ioeGetErrorType e)
>                 then Just ()
>                 else Nothing)
>             ((Just . fromByteString) `fmap` unsafeMMapFile infn)
>             (\() -> return Nothing)

	// Verify the shards
	ok, err := enc.Verify(shards)

>     ok <- catchJust
>             (\e -> case fromException e of
>                 Just RS.InvalidShardSize -> Just False
>                 _ -> Nothing)
>             (RS.verify RS.defaultBackend enc (V.map (fromMaybe V.empty) shards))
>             return

	if ok {
		fmt.Println("No reconstruction needed")

>     shards' <- if ok
>     then do
>         printf "No reconstruction needed\n"
>         return (V.map (fromMaybe V.empty) shards)

	} else {
		fmt.Println("Verification failed. Reconstructing data")

>     else do
>         printf "Verification failed. Reconstructing data\n"

		err = enc.Reconstruct(shards)
		if err != nil {
			fmt.Println("Reconstruct failed -", err)
			os.Exit(1)
		}

>         shards' <- RS.reconstruct RS.defaultBackend enc shards

		ok, err = enc.Verify(shards)

>         ok' <- RS.verify RS.defaultBackend enc shards'

		if !ok {
			fmt.Println("Verification failed after reconstruction, data likely corrupted.")
			os.Exit(1)
		}
		checkErr(err)

>         when (not ok') $
>             error "Verification failed after reconstruction, data likely corrupted."
>
>         return shards'

	}

	// Join the shards and write them
	outfn := *outFile
	if outfn == "" {
		outfn = fname
	}

>     let outfn = fromMaybe fname (optionsOut options)

	fmt.Println("Writing data to", outfn)

>     printf "Writing data to %s\n" outfn

	f, err := os.Create(outfn)
	checkErr(err)

>     withFile outfn WriteMode $ \f -> do

	// We don't know the exact filesize.
	err = enc.Join(f, shards, len(shards[0])**dataShards)
	checkErr(err)

>         V.forM_ (V.take dataShards shards') $
>             BS.hPut f . toByteString

}

func checkErr(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %s", err.Error())
		os.Exit(2)
	}
}
