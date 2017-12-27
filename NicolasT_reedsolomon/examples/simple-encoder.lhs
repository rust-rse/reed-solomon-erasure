> {-# LANGUAGE CPP #-}
> module Main (main) where
>
> import Control.Monad (forM_, when)
> import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4, 8, 0)
> import Data.Monoid (mempty)
#endif
> import Text.Printf (printf)
>
> import System.FilePath (joinPath, takeDirectory, takeFileName)
>
> import qualified Data.ByteString as BS
>
> import qualified Data.Vector.Generic as V
>
> import Options.Applicative
#if MIN_VERSION_optparse_applicative(0, 13, 0)
> import Data.Monoid ((<>))
#endif
>
> import System.IO.Posix.MMap (unsafeMMapFile)
>
> import Data.Vector.Storable.ByteString (fromByteString, toByteString)
>
> import qualified Data.ReedSolomon as RS

//+build ignore

// Copyright 2015, Klaus Post, see LICENSE for details.
//
// Simple encoder example
//
// The encoder encodes a simgle file into a number of shards
// To reverse the process see "simpledecoder.go"
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
	"path/filepath"

	"github.com/klauspost/reedsolomon"
)

var dataShards = flag.Int("data", 4, "Number of shards to split the data into, must be below 257.")
var parShards = flag.Int("par", 2, "Number of parity shards")
var outDir = flag.String("out", "", "Alternative output directory")

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
		fmt.Fprintf(os.Stderr, "  simple-encoder [-flags] filename.ext\n\n")
		fmt.Fprintf(os.Stderr, "Valid flags:\n")
		flag.PrintDefaults()
	}
}

func main() {

> main :: IO ()
> main = do

	// Parse command line parameters.
	flag.Parse()

>     options <- execParser $ info (helper <*> parser) mempty

	args := flag.Args()
	if len(args) != 1 {
		fmt.Fprintf(os.Stderr, "Error: No input filename given\n")
		flag.Usage()
		os.Exit(1)
	}
	if *dataShards > 257 {
		fmt.Fprintf(os.Stderr, "Error: Too many data shards\n")
		os.Exit(1)
	}

>     when (optionsData options > 256) $
>         error "Too many data shards"

	fname := args[0]

>     let fname = optionsFname options
>         dataShards = optionsData options
>         parShards = optionsPar options

	// Create encoding matrix.
	enc, err := reedsolomon.New(*dataShards, *parShards)
	checkErr(err)

>     enc <- RS.new dataShards parShards

	fmt.Println("Opening", fname)
	b, err := ioutil.ReadFile(fname)
	checkErr(err)

>     printf "Opening %s\n" fname
>     b <- fromByteString `fmap` unsafeMMapFile fname

	// Split the file into equally sized shards.
	shards, err := enc.Split(b)
	checkErr(err)

>     shards <- RS.split enc b

	fmt.Printf("File split into %d data+parity shards with %d bytes/shard.\n", len(shards), len(shards[0]))

>     printf "File split into %d data shards with %d bytes/shard.\n"
>         (V.length shards) (V.length $ V.head shards)

	// Encode parity
	err = enc.Encode(shards)
	checkErr(err)

>     parities <- RS.encode RS.defaultBackend enc shards
>     let shards' = (V.++) shards parities

	// Write out the resulting files.
	dir, file := filepath.Split(fname)
	if *outDir != "" {
		dir = *outDir
	}

>     let dir = fromMaybe (takeDirectory fname) (optionsOut options)
>         file = takeFileName fname

	for i, shard := range shards {
		outfn := fmt.Sprintf("%s.%d", file, i)

		fmt.Println("Writing to", outfn)
		err = ioutil.WriteFile(filepath.Join(dir, outfn), shard, os.ModePerm)
		checkErr(err)
	}

>     forM_ (zip [(0 :: Int)..] (V.toList shards')) $ \(i, shard) -> do
>         let outfn = concat [file, ".", show i]
>
>         printf "Writing to %s\n" outfn
>         BS.writeFile (joinPath [dir, outfn]) (toByteString shard)

}

func checkErr(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %s", err.Error())
		os.Exit(2)
	}
}
