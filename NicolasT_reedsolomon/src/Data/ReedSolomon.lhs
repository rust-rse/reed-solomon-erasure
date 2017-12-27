> {-# LANGUAGE CPP #-}
> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ == 708
> {-# OPTIONS_GHC -fno-warn-amp #-}
#endif
> {-# OPTIONS_HADDOCK show-extensions #-}
>
> -- |
> -- Module      : Data.ReedSolomon
> -- Description : Core Reed-Solomon encoding and reconstruction routines
> -- Copyright   : (C) 2015 Nicolas Trangez
> --                   2015 Klaus Post
> --                   2015 Backblaze
> -- License     : MIT (see the file LICENSE)
> -- Maintainer  : Nicolas Trangez <ikke@nicolast.be>
> -- Stability   : provisional
> --
> -- This module implements the core Reed-Solomon data encoding and
> -- reconstruction routines.
>
> module Data.ReedSolomon (
>     -- * Core API
>       Encoder
>     , defaultBackend
>     , new
>     , Matrix
>     , encode
>     , verify
>     , reconstruct
>     -- * Data conversion utilities
>     , split
>     , join
>     -- * Exceptions
>     , ShardType(..)
>     , ValueError(..)
>     -- * Determining runtime SIMD instruction support
>     , SIMDInstructions(..)
>     , simdInstructions
>     ) where
>
> import Prelude hiding (any)
>
> import Control.Exception.Base (Exception)
> import Control.Monad.ST (ST, runST)
> import Control.Monad.Trans (lift)
> import Data.Foldable (any)
> import Data.Maybe (fromJust, fromMaybe)
> import Data.Typeable (Typeable)
> import Data.Word
>
#if HAVE_SIMD
> import Foreign.C (CInt(..))
#endif
>
> import Control.Monad.Catch (MonadThrow, throwM)
>
> import qualified Data.Vector as V (Vector, MVector)
> import qualified Data.Vector.Generic as V hiding (Vector)
> import qualified Data.Vector.Generic.Mutable as MV
> import qualified Data.Vector.Storable as SV
>
> import Control.Loop (numLoop)
>
> import Data.ReedSolomon.Backend (Backend(..))
> import Data.ReedSolomon.Matrix (Matrix)
> import qualified Data.ReedSolomon.Matrix as Matrix
> import qualified Data.Vector.Generic.Compat as VC
> import qualified Data.Vector.Generic.Exceptions as VE
> import qualified Data.Vector.Generic.Lifted as VL
#if HAVE_SIMD
> import qualified Data.ReedSolomon.Galois.SIMD as B (backend)
#else
> import qualified Data.ReedSolomon.Galois.NoAsm as B (backend)
#endif

/**
 * Reed-Solomon Coding over 8-bit values.
 *
 * Copyright 2015, Klaus Post
 * Copyright 2015, Backblaze, Inc.
 */

// Reed-Solomon Erasure Coding in Go
//
// For usage and examples, see https://github.com/klauspost/reedsolomon
//
package reedsolomon

import (
	"bytes"
	"errors"
	"io"
	"runtime"
	"sync"
)

// Encoder is an interface to encode Reed-Salomon parity sets for your data.
type Encoder interface {
	// Encodes parity for a set of data shards.
	// Input is 'shards' containing data shards followed by parity shards.
	// The number of shards must match the number given to New().
	// Each shard is a byte array, and they must all be the same size.
	// The parity shards will always be overwritten and the data shards
	// will remain the same, so it is safe for you to read from the
	// data shards while this is running.
	Encode(shards [][]byte) error

	// Verify returns true if the parity shards contain correct data.
	// The data is the same format as Encode. No data is modified, so
	// you are allowed to read from data while this is running.
	Verify(shards [][]byte) (bool, error)

	// Reconstruct will recreate the missing shards if possible.
	//
	// Given a list of shards, some of which contain data, fills in the
	// ones that don't have data.
	//
	// The length of the array must be equal to the total number of shards.
	// You indicate that a shard is missing by setting it to nil.
	//
	// If there are too few shards to reconstruct the missing
	// ones, ErrTooFewShards will be returned.
	//
	// The reconstructed shard set is complete, but integrity is not verified.
	// Use the Verify function to check if data set is ok.
	Reconstruct(shards [][]byte) error

	// Split a data slice into the number of shards given to the encoder,
	// and create empty parity shards.
	//
	// The data will be split into equally sized shards.
	// If the data size isn't dividable by the number of shards,
	// the last shard will contain extra zeros.
	//
	// There must be at least the same number of bytes as there are data shards,
	// otherwise ErrShortData will be returned.
	//
	// The data will not be copied, except for the last shard, so you
	// should not modify the data of the input slice afterwards.
	Split(data []byte) ([][]byte, error)

	// Join the shards and write the data segment to dst.
	//
	// Only the data shards are considered.
	// You must supply the exact output size you want.
	// If there are to few shards given, ErrTooFewShards will be returned.
	// If the total data size is less than outSize, ErrShortData will be returned.
	Join(dst io.Writer, shards [][]byte, outSize int) error
}

// reedSolomon contains a matrix for a specific
// distribution of datashards and parity shards.
// Construct if using New()
type reedSolomon struct {
	DataShards   int // Number of data shards, should not be modified.
	ParityShards int // Number of parity shards, should not be modified.
	Shards       int // Total number of shards. Calculated, and should not be modified.
	m            matrix
	parity       [][]byte
}

> -- | A Reed-Solomon encoder.
> data Encoder = Encoder { rsDataShards :: {-# UNPACK #-} !Int
>                        , rsParityShards :: {-# UNPACK #-} !Int
>                        , rsM :: Matrix
>                        }
>   deriving (Show, Eq, Typeable)
>
> rsShards :: Encoder -> Int
> rsShards enc = rsDataShards enc + rsParityShards enc
>
> rsParity :: Encoder -> Matrix
> rsParity enc = V.drop (rsDataShards enc) (rsM enc)

> type MMatrix s = V.Vector (SV.MVector s Word8)

> -- | Enumeration of kinds of shards.
> data ShardType = DataShard | ParityShard | AnyShard
>   deriving (Show, Eq, Bounded, Enum, Typeable)
>
> -- | Exception type used to denote invalid input.
> data ValueError = InvalidNumberOfShards ShardType Int -- ^ Invalid number of shards
>                 | EmptyShards -- ^ All shards are empty
>                 | InvalidShardSize -- ^ A shard of invalid size was passed
>                 | InvalidDataSize -- ^ The length of the passed data is invalid
>   deriving (Show, Eq, Typeable)
> instance Exception ValueError

// ErrInvShardNum will be returned by New, if you attempt to create
// an Encoder where either data or parity shards is zero or less,
// or the number of data shards is higher than 256.
var ErrInvShardNum = errors.New("cannot create Encoder with zero or less data/parity shards")

// New creates a new encoder and initializes it to
// the number of data shards and parity shards that
// you want to use. You can reuse this encoder.
// Note that the maximum number of data shards is 256.
func New(dataShards, parityShards int) (Encoder, error) {
	r := reedSolomon{
		DataShards:   dataShards,
		ParityShards: parityShards,
		Shards:       dataShards + parityShards,
	}

	if dataShards <= 0 || parityShards <= 0 {
		return nil, ErrInvShardNum
	}

	if dataShards > 256 {
		return nil, ErrInvShardNum
	}

	// Start with a Vandermonde matrix.  This matrix would work,
	// in theory, but doesn't have the property that the data
	// shards are unchanged after encoding.
	vm, err := vandermonde(r.Shards, dataShards)
	if err != nil {
		return nil, err
	}

	// Multiply by the inverse of the top square of the matrix.
	// This will make the top square be the identity matrix, but
	// preserve the property that any square subset of rows  is
	// invertible.
	top, _ := vm.SubMatrix(0, 0, dataShards, dataShards)
	top, _ = top.Invert()
	r.m, _ = vm.Multiply(top)

	r.parity = make([][]byte, parityShards)
	for i := range r.parity {
		r.parity[i] = r.m[dataShards+i]
	}

	return &r, err
}

> -- | Construct a new 'Encoder'.
> --
> -- Throws 'InvalidNumberOfShards' when 'dataShards' or 'parityShards' is out
> -- of range.
> --
> -- An 'Encoder' can safely be reused between encoding and reconstruction
> -- rounds, as well as shared between threads.
> new :: MonadThrow m
>     => Int -- ^ Number of data shards ([0, 255])
>     -> Int -- ^ Number of parity shards ([0, ])
>     -> m Encoder
> new dataShards parityShards
>     | dataShards <= 0 = throwM (InvalidNumberOfShards DataShard dataShards)
>     | parityShards <= 0 = throwM (InvalidNumberOfShards ParityShard parityShards)
>     | dataShards > 256 = throwM (InvalidNumberOfShards DataShard dataShards)
>     | otherwise = do
>         let shards = dataShards + parityShards
>         let vm = Matrix.vandermonde shards dataShards
>         let top = Matrix.subMatrix vm 0 0 dataShards dataShards
>         top' <- Matrix.invert top
>         m <- Matrix.multiply vm top'
>
>         return $ Encoder { rsDataShards = dataShards
>                          , rsParityShards = parityShards
>                          , rsM = m
>                          }

// ErrTooFewShards is returned if too few shards where given to
// Encode/Verify/Reconstruct. It will also be returned from Reconstruct
// if there were too few shards to reconstruct the missing data.
var ErrTooFewShards = errors.New("too few shards given")

// Encodes parity for a set of data shards.
// An array 'shards' containing data shards followed by parity shards.
// The number of shards must match the number given to New.
// Each shard is a byte array, and they must all be the same size.
// The parity shards will always be overwritten and the data shards
// will remain the same.
func (r reedSolomon) Encode(shards [][]byte) error {
	if len(shards) != r.Shards {
		return ErrTooFewShards
	}

	err := checkShards(shards, false)
	if err != nil {
		return err
	}

	// Get the slice of output buffers.
	output := shards[r.DataShards:]

	// Do the coding.
	r.codeSomeShards(r.parity, shards[0:r.DataShards], output, r.ParityShards, len(shards[0]))
	return nil
}

> -- | Encode some data shards into parity shards.
> --
> -- The number of rows in the given matrix must match the number of data shards
> -- the 'Encoder' requires, otherwise 'InvalidNumberOfShards' is thrown.
> -- If all shards are empty, 'EmptyShards' is thrown. Similarly, when shards
> -- are of inconsistent length, 'InvalidShardSize' is thrown.
> encode :: MonadThrow m
>        => (forall s. Backend s) -- ^ Encoding backend to use
>        -> Encoder -- ^ Encoder to use
>        -> Matrix -- ^ Data shards
>        -> m Matrix
> encode backend r shards
>     | V.length shards /= rsDataShards r = throwM (InvalidNumberOfShards DataShard $ V.length shards)
>     | Left e <- checkShards shards False = throwM e
>     | otherwise = return $ runST $ do
>         output :: MMatrix s <- V.replicateM (rsParityShards r) (MV.new $ shardSize shards)
>         codeSomeShards backend r (rsParity r) shards output (rsParityShards r) (V.length $ V.head shards)
>         V.forM output V.unsafeFreeze

// Verify returns true if the parity shards contain the right data.
// The data is the same format as Encode. No data is modified.
func (r reedSolomon) Verify(shards [][]byte) (bool, error) {
	if len(shards) != r.Shards {
		return false, ErrTooFewShards
	}
	err := checkShards(shards, false)
	if err != nil {
		return false, err
	}

	// Slice of buffers being checked.
	toCheck := shards[r.DataShards:]

	// Do the checking.
	return r.checkSomeShards(r.parity, shards[0:r.DataShards], toCheck, r.ParityShards, len(shards[0])), nil
}

> -- | Verify the code chunks against the data.
> --
> -- This function will re-encode the data chunks, and validate the code chunks
> -- in the given 'Matrix' are equal.
> verify :: MonadThrow m => (forall s. Backend s) -> Encoder -> Matrix -> m Bool
> verify backend r shards
>     | V.length shards /= rsShards r = throwM (InvalidNumberOfShards DataShard $ V.length shards)
>     | Left e <- checkShards shards False = throwM e
>     | otherwise =
>         let (dataShards, toCheck) = V.splitAt (rsDataShards r) shards in
>         checkSomeShards backend r (rsParity r) dataShards toCheck (rsParityShards r) (V.length $ V.head shards)

// Multiplies a subset of rows from a coding matrix by a full set of
// input shards to produce some output shards.
// 'matrixRows' is The rows from the matrix to use.
// 'inputs' An array of byte arrays, each of which is one input shard.
// The number of inputs used is determined by the length of each matrix row.
// outputs Byte arrays where the computed shards are stored.
// The number of outputs computed, and the
// number of matrix rows used, is determined by
// outputCount, which is the number of outputs to compute.
func (r reedSolomon) codeSomeShards(matrixRows, inputs, outputs [][]byte, outputCount, byteCount int) {
	if runtime.GOMAXPROCS(0) > 1 && len(inputs[0]) > splitSize {
		r.codeSomeShardsP(matrixRows, inputs, outputs, outputCount, byteCount)
		return
	}
	for c := 0; c < r.DataShards; c++ {
		in := inputs[c]
		for iRow := 0; iRow < outputCount; iRow++ {
			if c == 0 {
				galMulSlice(matrixRows[iRow][c], in, outputs[iRow])
			} else {
				galMulSliceXor(matrixRows[iRow][c], in, outputs[iRow])
			}
		}
	}
}

> codeSomeShards :: Backend s -> Encoder -> Matrix -> Matrix -> MMatrix s -> Int -> Int -> ST s ()
> codeSomeShards backend _r matrixRows inputs outputs _outputCount _byteCount = do
>     VC.iforM_ inputs $ \c in_ -> {-# SCC "codeSomeShards:dataShards" #-} do
>         VC.iforM_ outputs $ \iRow oi -> {-# SCC "codeSomeShards:parityShards" #-} do
>             let mic = V.unsafeIndex (V.unsafeIndex matrixRows iRow) c
>             if (c == 0)
>             then {-# SCC "codeSomeShards:galMulSlice" #-} (backendGalMulSlice backend) mic in_ oi
>             else {-# SCC "codeSomeShards:galMulSliceXor" #-} (backendGalMulSliceXor backend) mic in_ oi
> {-# INLINE codeSomeShards #-}

// How many bytes per goroutine.
const splitSize = 512

// Perform the same as codeSomeShards, but split the workload into
// several goroutines.
func (r reedSolomon) codeSomeShardsP(matrixRows, inputs, outputs [][]byte, outputCount, byteCount int) {
	var wg sync.WaitGroup
	left := byteCount
	start := 0
	for {
		do := left
		if do > splitSize {
			do = splitSize
		}
		if do == 0 {
			break
		}
		left -= do
		wg.Add(1)
		go func(start, stop int) {
			for c := 0; c < r.DataShards; c++ {
				in := inputs[c]
				for iRow := 0; iRow < outputCount; iRow++ {
					if c == 0 {
						galMulSlice(matrixRows[iRow][c], in[start:stop], outputs[iRow][start:stop])
					} else {
						galMulSliceXor(matrixRows[iRow][c], in[start:stop], outputs[iRow][start:stop])
					}
				}
			}
			wg.Done()
		}(start, start+do)
		start += do
	}
	wg.Wait()
}

// checkSomeShards is mostly the same as codeSomeShards,
// except this will check values and return
// as soon as a difference is found.
func (r reedSolomon) checkSomeShards(matrixRows, inputs, toCheck [][]byte, outputCount, byteCount int) bool {
	var wg sync.WaitGroup
	left := byteCount
	start := 0

	same := true
	var mu sync.RWMutex // For above

	for {
		do := left
		if do > splitSize {
			do = splitSize
		}
		if do == 0 {
			break
		}
		left -= do
		wg.Add(1)
		go func(start, do int) {
			defer wg.Done()
			outputs := make([][]byte, len(toCheck))
			for i := range outputs {
				outputs[i] = make([]byte, do)
			}
			for c := 0; c < r.DataShards; c++ {
				mu.RLock()
				if !same {
					mu.RUnlock()
					return
				}
				mu.RUnlock()
				in := inputs[c][start : start+do]
				for iRow := 0; iRow < outputCount; iRow++ {
					galMulSliceXor(matrixRows[iRow][c], in, outputs[iRow])
				}
			}

			for i, calc := range outputs {
				if bytes.Compare(calc, toCheck[i][start:start+do]) != 0 {
					mu.Lock()
					same = false
					mu.Unlock()
					return
				}
			}
		}(start, do)
		start += do
	}
	wg.Wait()
	return same
}

> checkSomeShards :: MonadThrow m => (forall s. Backend s) -> Encoder -> Matrix -> Matrix -> Matrix -> Int -> Int -> m Bool
> checkSomeShards backend r _matrixRows inputs toCheck _outputCount _byteCount =
>     -- Very basic non-parallel implementation for now
>     encode backend r inputs >>= \encoded -> return $ encoded == toCheck

// ErrShardNoData will be returned if there are no shards,
// or if the length of all shards is zero.
var ErrShardNoData = errors.New("no shard data")

// ErrShardSize is returned if shard length isn't the same for all
// shards.
var ErrShardSize = errors.New("shard sizes does not match")

// checkShards will check if shards are the same size
// or 0, if allowed. An error is returned if this fails.
// An error is also returned if all shards are size 0.
func checkShards(shards [][]byte, nilok bool) error {
	size := shardSize(shards)
	if size == 0 {
		return ErrShardNoData
	}
	for _, shard := range shards {
		if len(shard) != size {
			if len(shard) != 0 || !nilok {
				return ErrShardSize
			}
		}
	}
	return nil
}

> checkShards :: Matrix -> Bool -> Either ValueError ()
> checkShards shards nilok
>     | size == 0 = Left EmptyShards
>     | otherwise =
>         if any invalid shards
>         then Left InvalidShardSize
>         else Right ()
>   where
>     size = shardSize shards
>     invalid shard
>         | V.length shard /= size =
>             if (V.length shard /= 0) || not nilok
>             then True
>             else False
>         | otherwise = False

// shardSize return the size of a single shard.
// The first non-zero size is returned,
// or 0 if all shards are size 0.
func shardSize(shards [][]byte) int {
	for _, shard := range shards {
		if len(shard) != 0 {
			return len(shard)
		}
	}
	return 0
}

> shardSize :: Matrix -> Int
> shardSize shards = maybe 0 V.length $ V.find (\v -> V.length v /= 0) shards

// Reconstruct will recreate the missing shards, if possible.
//
// Given a list of shards, some of which contain data, fills in the
// ones that don't have data.
//
// The length of the array must be equal to Shards.
// You indicate that a shard is missing by setting it to nil.
//
// If there are too few shards to reconstruct the missing
// ones, ErrTooFewShards will be returned.
//
// The reconstructed shard set is complete, but integrity is not verified.
// Use the Verify function to check if data set is ok.
func (r reedSolomon) Reconstruct(shards [][]byte) error {
	if len(shards) != r.Shards {
		return ErrTooFewShards
	}
	// Check arguments.
	err := checkShards(shards, true)
	if err != nil {
		return err
	}

	shardSize := shardSize(shards)

	// Quick check: are all of the shards present?  If so, there's
	// nothing to do.
	numberPresent := 0
	for i := 0; i < r.Shards; i++ {
		if len(shards[i]) != 0 {
			numberPresent++
		}
	}
	if numberPresent == r.Shards {
		// Cool.  All of the shards data data.  We don't
		// need to do anything.
		return nil
	}

	// More complete sanity check
	if numberPresent < r.DataShards {
		return ErrTooFewShards
	}

	// Pull out the rows of the matrix that correspond to the
	// shards that we have and build a square matrix.  This
	// matrix could be used to generate the shards that we have
	// from the original data.
	//
	// Also, pull out an array holding just the shards that
	// correspond to the rows of the submatrix.  These shards
	// will be the input to the decoding process that re-creates
	// the missing data shards.
	subMatrix, _ := newMatrix(r.DataShards, r.DataShards)
	subShards := make([][]byte, r.DataShards)
	subMatrixRow := 0
	for matrixRow := 0; matrixRow < r.Shards && subMatrixRow < r.DataShards; matrixRow++ {
		if len(shards[matrixRow]) != 0 {
			for c := 0; c < r.DataShards; c++ {
				subMatrix[subMatrixRow][c] = r.m[matrixRow][c]
			}
			subShards[subMatrixRow] = shards[matrixRow]
			subMatrixRow++
		}
	}

	// Invert the matrix, so we can go from the encoded shards
	// back to the original data.  Then pull out the row that
	// generates the shard that we want to decode.  Note that
	// since this matrix maps back to the original data, it can
	// be used to create a data shard, but not a parity shard.
	dataDecodeMatrix, err := subMatrix.Invert()
	if err != nil {
		return err
	}

	// Re-create any data shards that were missing.
	//
	// The input to the coding is all of the shards we actually
	// have, and the output is the missing data shards.  The computation
	// is done using the special decode matrix we just built.
	outputs := make([][]byte, r.ParityShards)
	matrixRows := make([][]byte, r.ParityShards)
	outputCount := 0

	for iShard := 0; iShard < r.DataShards; iShard++ {
		if len(shards[iShard]) == 0 {
			shards[iShard] = make([]byte, shardSize)
			outputs[outputCount] = shards[iShard]
			matrixRows[outputCount] = dataDecodeMatrix[iShard]
			outputCount++
		}
	}
	r.codeSomeShards(matrixRows, subShards, outputs[:outputCount], outputCount, shardSize)

	// Now that we have all of the data shards intact, we can
	// compute any of the parity that is missing.
	//
	// The input to the coding is ALL of the data shards, including
	// any that we just calculated.  The output is whichever of the
	// data shards were missing.
	outputCount = 0
	for iShard := r.DataShards; iShard < r.Shards; iShard++ {
		if len(shards[iShard]) == 0 {
			shards[iShard] = make([]byte, shardSize)
			outputs[outputCount] = shards[iShard]
			matrixRows[outputCount] = r.parity[iShard-r.DataShards]
			outputCount++
		}
	}
	r.codeSomeShards(matrixRows, shards[:r.DataShards], outputs[:outputCount], outputCount, shardSize)
	return nil
}

> -- | Reconstruct partial data.
> reconstruct :: MonadThrow m
>             => (forall s. Backend s) -- ^ Encoding backend to use
>             -> Encoder -- ^ Encoder to use
>             -> V.Vector (Maybe (SV.Vector Word8)) -- ^ Partial data and parity shards
>             -> m Matrix -- ^ Reconstructed data and parity shards
> reconstruct backend r shards0
>     | V.length shards0 /= rsShards r = throwM (InvalidNumberOfShards AnyShard $ V.length shards0)
>     | Left e <- checkShards shards0' True = throwM e
>     | numberPresent == rsShards r = return $ V.map fromJust shards0
>     | numberPresent < rsDataShards r = throwM (InvalidNumberOfShards AnyShard numberPresent)
>     | otherwise = VE.runCatchST $ do
>         shards :: V.MVector s (SV.MVector s Word8) <- VL.new (V.length shards0)
>         numLoop 0 (MV.length shards - 1) $ \i -> do
>             let row = (V.!) shards0 i
>             row' <- maybe (VL.new 0) VL.thaw row
>             VL.write shards i row'
>
>         subMatrix :: V.Vector (SV.MVector s Word8) <- V.replicateM dataShards (VL.replicate dataShards 0)
>         subShards :: V.MVector s (SV.MVector s Word8) <- VL.new dataShards
>
>         let loop0 matrixRow subMatrixRow
>                 | matrixRow >= rsShards r = return ()
>                 | subMatrixRow >= dataShards = return ()
>                 | otherwise = do
>                     shardsmatrixRow <- VL.read shards matrixRow
>                     if (MV.length shardsmatrixRow) /= 0
>                     then do
>                         numLoop 0 (dataShards - 1) $ \c -> do
>                             let mmatrixRowc = (V.!) ((V.!) (rsM r) matrixRow) c
>                             let subMatrixsubMatrixRow = (V.!) subMatrix subMatrixRow
>                             VL.write subMatrixsubMatrixRow c mmatrixRowc
>                         VL.write subShards subMatrixRow shardsmatrixRow
>                         loop0 (matrixRow + 1) (subMatrixRow + 1)
>                     else
>                         loop0 (matrixRow + 1) subMatrixRow
>
>         loop0 0 0
>
>         dataDecodeMatrix <- Matrix.invert =<< VL.forM subMatrix V.freeze
>
>         outputs :: V.MVector s (SV.MVector s Word8) <- VL.new (rsParityShards r)
>         matrixRows :: V.MVector s (SV.Vector Word8) <- VL.new (rsParityShards r)
>
>         let loop1 iShard outputCount
>                 | iShard >= dataShards = return outputCount
>                 | otherwise = do
>                     shardsiShard0 <- VL.read shards iShard
>                     if (MV.length shardsiShard0 == 0)
>                     then do
>                         shardsiShard <- VL.new shardSize'
>                         VL.write shards iShard shardsiShard
>                         VL.write outputs outputCount shardsiShard
>                         VL.write matrixRows outputCount ((V.!) dataDecodeMatrix iShard)
>                         loop1 (iShard + 1) (outputCount + 1)
>                     else
>                         loop1 (iShard + 1) outputCount
>
>         outputCount <- loop1 0 0
>
>         matrixRows' <- VL.freeze matrixRows
>         outputs' <- VL.freeze outputs
>
>         subShards' <- VL.mapM V.freeze =<< VL.freeze subShards
>         lift $ codeSomeShards backend r matrixRows' subShards' (V.take outputCount outputs') outputCount shardSize'
>
>         let loop2 iShard outputCount'
>                 | iShard >= rsShards r = return outputCount'
>                 | otherwise = do
>                     shardsiShard0 <- VL.read shards iShard
>                     if (MV.length shardsiShard0 == 0)
>                     then do
>                         shardsiShard <- VL.new shardSize'
>                         VL.write shards iShard shardsiShard
>                         VL.write outputs outputCount' shardsiShard
>                         let p = (V.!) (rsParity r) (iShard - dataShards)
>                         VL.write matrixRows outputCount' p
>                         loop2 (iShard + 1) (outputCount' + 1)
>                     else
>                         loop2 (iShard + 1) outputCount'
>
>         outputCount' <- loop2 0 0
>
>         matrixRows'' <- VL.freeze matrixRows
>         outputs'' <- VL.freeze outputs
>         s <- VL.mapM V.freeze =<< (VL.freeze $ MV.slice 0 dataShards shards)
>
>         lift $ codeSomeShards backend r matrixRows'' s (V.slice 0 outputCount' outputs'') outputCount' shardSize'
>
>         VL.mapM V.unsafeFreeze =<< VL.unsafeFreeze shards
>   where
>     shards0' = V.map (fromMaybe V.empty) shards0
>     numberPresent = V.foldr (\v c -> maybe c (const $ c + 1) v) 0 shards0
>     dataShards = rsDataShards r
>     shardSize' = shardSize shards0'

// ErrShortData will be returned by Split(), if there isn't enough data
// to fill the number of shards.
var ErrShortData = errors.New("not enough data to fill the number of requested shards")

// Split a data slice into the number of shards given to the encoder,
// and create empty parity shards.
//
// The data will be split into equally sized shards.
// If the data size isn't divisible by the number of shards,
// the last shard will contain extra zeros.
//
// There must be at least the same number of bytes as there are data shards,
// otherwise ErrShortData will be returned.
//
// The data will not be copied, except for the last shard, so you
// should not modify the data of the input slice afterwards.
func (r reedSolomon) Split(data []byte) ([][]byte, error) {
	if len(data) < r.DataShards {
		return nil, ErrShortData
	}

	// Calculate number of bytes per shard.
	perShard := (len(data) + r.DataShards - 1) / r.DataShards

	// Pad data to r.Shards*perShard.
	padding := make([]byte, (r.Shards*perShard)-len(data))
	data = append(data, padding...)

	// Split into equal-length shards.
	dst := make([][]byte, r.Shards)
	for i := range dst {
		dst[i] = data[:perShard]
		data = data[perShard:]
	}

	return dst, nil
}

> -- | Split data into shards that can be encoded by the given 'Encoder'.
> --
> -- If required, the last shard is padded with zeros to match size.
> split :: MonadThrow m => Encoder -> SV.Vector Word8 -> m Matrix
> split r data_
>     | V.length data_ < rsDataShards r = throwM InvalidDataSize
>     | otherwise =
>         let perShard = (V.length data_ + rsDataShards r - 1) `div` rsDataShards r in
>         -- Note: rsDataShards instead of rsShards: we don't add space for parity
>         let padding = V.replicate (rsDataShards r * perShard - V.length data_) 0 in
>         return $ flip (V.unfoldrN (rsDataShards r)) (data_, padding) $ \(rest, padding') ->
>             if V.null rest
>             then
>                 if V.null padding'
>                 then Nothing
>                 else
>                     let (h, t) = V.splitAt perShard padding' in
>                     Just (h, (rest, t))
>             else
>                 if V.length rest >= perShard
>                 then
>                     let (h, t) = V.splitAt perShard rest in
>                     Just (h, (t, padding'))
>                 else
>                     let (h, t) = V.splitAt (perShard - V.length rest) padding' in
>                     Just ((V.++) rest h, (V.empty, t))

// Join the shards and write the data segment to dst.
//
// Only the data shards are considered.
// You must supply the exact output size you want.
// If there are to few shards given, ErrTooFewShards will be returned.
// If the total data size is less than outSize, ErrShortData will be returned.
func (r reedSolomon) Join(dst io.Writer, shards [][]byte, outSize int) error {
	// Do we have enough shards?
	if len(shards) < r.DataShards {
		return ErrTooFewShards
	}
	shards = shards[:r.DataShards]

	// Do we have enough data?
	size := 0
	for _, shard := range shards {
		size += len(shard)
	}
	if size < outSize {
		return ErrShortData
	}

	// Copy data to dst
	write := outSize
	for _, shard := range shards {
		if write < len(shard) {
			_, err := dst.Write(shard[:write])
			return err
		}
		n, err := dst.Write(shard)
		if err != nil {
			return err
		}
		write -= n
	}
	return nil
}

> -- | Join shards into a single 'Data.Vector.Storable.Vector' of given size.
> --
> -- Note: This function concatenates all data through copies, which is
> -- inefficient. When writing the data to a file or socket, consider doing
> -- so without joining in-memory.
> join :: MonadThrow m => Encoder -> Matrix -> Int -> m (SV.Vector Word8)
> join r shards outSize
>     | V.length shards < rsDataShards r = throwM (InvalidNumberOfShards DataShard $ V.length shards)
>     | V.sum (V.map V.length shards) < outSize = throwM InvalidDataSize
>     | otherwise = return $ V.take outSize $ V.concat $ V.toList shards

> -- | Enumeration of SIMD instruction sets.
> data SIMDInstructions = Generic
>                       | SSE2
>                       | SSSE3
>                       | AVX
>                       | AVX2
>                       | NEON
>                       | AltiVec
>   deriving (Show, Eq, Ord, Enum, Bounded)
>
> -- | Retrieve the SIMD instruction set used by 'native' Galois field operations.
> --
> -- Returns 'Nothing' when the library is compiled without SIMD support,
> -- otherwise 'Just' the runtime-determined SIMD instruction set used for
> -- optimized Galois field calculations.
> simdInstructions :: IO (Maybe SIMDInstructions)
> simdInstructions =
#if !HAVE_SIMD
>       return Nothing
#else
>       (Just . toEnum . fromIntegral) `fmap` c_reedsolomon_determine_cpu_support
>
> foreign import ccall unsafe "reedsolomon_determine_cpu_support" c_reedsolomon_determine_cpu_support :: IO CInt
#endif

> -- | Default encoding routine backend.
> --
> -- On systems where the SIMD routines are available, this will be the SIMD
> -- backend. On other systems, the native (but slow) Haskell implementation is
> -- selected.
> defaultBackend :: Backend s
> defaultBackend = B.backend
