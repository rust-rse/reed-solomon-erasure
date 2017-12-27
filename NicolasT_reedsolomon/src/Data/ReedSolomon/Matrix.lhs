> {-# LANGUAGE DeriveDataTypeable #-}
> module Data.ReedSolomon.Matrix (
>       Matrix
>     , identityMatrix
>     , multiply
>     , subMatrix
>     , invert
>     , vandermonde
>     ) where
>
> import Prelude hiding (break)
>
> import Control.Exception.Base (Exception)
> import Control.Monad (unless, when)
> import Control.Monad.ST (ST)
> import Control.Monad.Trans (lift)
> import Data.Bits (xor)
> import Data.STRef (newSTRef, readSTRef, writeSTRef)
> import Data.Typeable (Typeable)
> import Data.Word (Word8)
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
> import qualified Data.ReedSolomon.Galois as Galois
> import Data.Vector.Generic.Exceptions (CatchST)
> import qualified Data.Vector.Generic.Exceptions as VE
> import qualified Data.Vector.Generic.Lifted as VL

/**
 * Matrix Algebra over an 8-bit Galois Field
 *
 * Copyright 2015, Klaus Post
 * Copyright 2015, Backblaze, Inc.
 */

package reedsolomon

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

// byte[row][col]
type matrix [][]byte

> -- | A row-major 'Matrix' of bytes.
> type Matrix = V.Vector (SV.Vector Word8)
>
> matrixSize :: Matrix -> (Int, Int)
> matrixSize m = (V.length m, V.length (V.head m))

// newMatrix returns a matrix of zeros.
func newMatrix(rows, cols int) (matrix, error) {
	if rows <= 0 {
		return nil, errInvalidRowSize
	}
	if cols <= 0 {
		return nil, errInvalidColSize
	}

	m := matrix(make([][]byte, rows))
	for i := range m {
		m[i] = make([]byte, cols)
	}
	return m, nil
}

// NewMatrixData initializes a matrix with the given row-major data.
// Note that data is not copied from input.
func newMatrixData(data [][]byte) (matrix, error) {
	m := matrix(data)
	err := m.Check()
	if err != nil {
		return nil, err
	}
	return m, nil
}

// IdentityMatrix returns an identity matrix of the given size.
func identityMatrix(size int) (matrix, error) {
	m, err := newMatrix(size, size)
	if err != nil {
		return nil, err
	}
	for i := range m {
		m[i][i] = 1
	}
	return m, nil
}

> identityMatrix :: Int -> Matrix
> identityMatrix size =
>     V.generate size $ \i ->
>         V.generate size $ \j ->
>             if i == j
>             then 1
>             else 0

// errInvalidRowSize will be returned if attempting to create a matrix with negative or zero row number.
var errInvalidRowSize = errors.New("invalid row size")

// errInvalidColSize will be returned if attempting to create a matrix with negative or zero column number.
var errInvalidColSize = errors.New("invalid column size")

// errColSizeMismatch is returned if the size of matrix columns mismatch.
var errColSizeMismatch = errors.New("column size is not the same for all rows")

> data DimensionMismatch = DimensionMismatch String
>   deriving (Show, Eq, Typeable)
> instance Exception DimensionMismatch
>
> throwDimensionMismatch :: MonadThrow m => String -> Matrix -> Matrix -> m a
> throwDimensionMismatch f m1 m2 = throwM $ DimensionMismatch message
>   where
>     message = unwords [ "Can't", f
>                       , "matrix of size", show (matrixSize m1)
>                       , "with matrix of size", show (matrixSize m2)
>                       ]

func (m matrix) Check() error {
	rows := len(m)
	if rows <= 0 {
		return errInvalidRowSize
	}
	cols := len(m[0])
	if cols <= 0 {
		return errInvalidColSize
	}

	for _, col := range m {
		if len(col) != cols {
			return errColSizeMismatch
		}
	}
	return nil
}

// String returns a human-readable string of the matrix contents.
//
// Example: [[1, 2], [3, 4]]
func (m matrix) String() string {
	var rowOut []string
	for _, row := range m {
		var colOut []string
		for _, col := range row {
			colOut = append(colOut, strconv.Itoa(int(col)))
		}
		rowOut = append(rowOut, "["+strings.Join(colOut, ", ")+"]")
	}
	return "[" + strings.Join(rowOut, ", ") + "]"
}

// Multiply multiplies this matrix (the one on the left) by another
// matrix (the one on the right) and returns a new matrix with the result.
func (m matrix) Multiply(right matrix) (matrix, error) {
	if len(m[0]) != len(right) {
		return nil, fmt.Errorf("columns on left (%d) is different than rows on right (%d)", len(m[0]), len(right))
	}
	result, _ := newMatrix(len(m), len(right[0]))
	for r, row := range result {
		for c := range row {
			var value byte
			for i := range m[0] {
				value ^= galMultiply(m[r][i], right[i][c])
			}
			result[r][c] = value
		}
	}
	return result, nil
}

> multiply :: MonadThrow m => Matrix -> Matrix -> m Matrix
> multiply m right
>     | V.length (V.head m) /= V.length right = throwDimensionMismatch "multiply" m right
>     | otherwise = return $
>         V.generate (V.length m) $ \r ->
>             V.generate (V.length (V.head right)) $ \c ->
>                 foldr
>                     (\i value ->
>                         let mri = V.unsafeIndex (V.unsafeIndex m r) i in
>                         let rightic = V.unsafeIndex (V.unsafeIndex right i) c in
>                         value `xor` Galois.galMultiply mri rightic)
>                     0
>                     [0 .. V.length (V.head m) - 1]

// Augment returns the concatenation of this matrix and the matrix on the right.
func (m matrix) Augment(right matrix) (matrix, error) {
	if len(m) != len(right) {
		return nil, errMatrixSize
	}

	result, _ := newMatrix(len(m), len(m[0])+len(right[0]))
	for r, row := range m {
		for c := range row {
			result[r][c] = m[r][c]
		}
		cols := len(m[0])
		for c := range right[0] {
			result[r][cols+c] = right[r][c]
		}
	}
	return result, nil
}

> augment :: MonadThrow m => Matrix -> Matrix -> m Matrix
> augment m right
>     | V.length m /= V.length right = throwDimensionMismatch "augment" m right
>     | otherwise = return $ V.zipWith (V.++) m right

// errMatrixSize is returned if matrix dimensions are doesn't match.
var errMatrixSize = errors.New("matrix sizes does not match")

func (m matrix) SameSize(n matrix) error {
	if len(m) != len(n) {
		return errMatrixSize
	}
	for i := range m {
		if len(m[i]) != len(n[i]) {
			return errMatrixSize
		}
	}
	return nil
}

// Returns a part of this matrix. Data is copied.
func (m matrix) SubMatrix(rmin, cmin, rmax, cmax int) (matrix, error) {
	result, err := newMatrix(rmax-rmin, cmax-cmin)
	if err != nil {
		return nil, err
	}
	// OPTME: If used heavily, use copy function to copy slice
	for r := rmin; r < rmax; r++ {
		for c := cmin; c < cmax; c++ {
			result[r-rmin][c-cmin] = m[r][c]
		}
	}
	return result, nil
}

> subMatrix :: Matrix -> Int -> Int -> Int -> Int -> Matrix
> subMatrix m rmin cmin rmax cmax =
>     V.generate (rmax - rmin) $ \r ->
>         V.generate (cmax - cmin) $ \c ->
>             V.unsafeIndex (V.unsafeIndex m (rmin + r)) (cmin + c)

// SwapRows Exchanges two rows in the matrix.
func (m matrix) SwapRows(r1, r2 int) error {
	if r1 < 0 || len(m) <= r1 || r2 < 0 || len(m) <= r2 {
		return errInvalidRowSize
	}
	m[r2], m[r1] = m[r1], m[r2]
	return nil
}

> swapRows :: V.MVector s a -> Int -> Int -> ST s ()
> swapRows = MV.swap


// IsSquare will return true if the matrix is square
// and nil if the matrix is square
func (m matrix) IsSquare() bool {
	if len(m) != len(m[0]) {
		return false
	}
	return true
}

> isSquare :: Matrix -> Bool
> isSquare m
>     | V.length m /= V.length (V.head m) = False
>     | otherwise = True

// errSingular is returned if the matrix is singular and cannot be inversed
var errSingular = errors.New("matrix is singular")

> data SingularMatrix = SingularMatrix
>   deriving (Show, Eq, Typeable)
> instance Exception SingularMatrix

// errNotSquare is returned if attempting to inverse a non-square matrix.
var errNotSquare = errors.New("only square matrices can be inverted")

// Invert returns the inverse of this matrix.
// Returns ErrSingular when the matrix is singular and doesn't have an inverse.
// The matrix must be square, otherwise ErrNotSquare is returned.
func (m matrix) Invert() (matrix, error) {
	if !m.IsSquare() {
		return nil, errNotSquare
	}

	size := len(m)
	work, _ := identityMatrix(size)
	work, _ = m.Augment(work)

	err := work.gaussianElimination()
	if err != nil {
		return nil, err
	}

	return work.SubMatrix(0, size, size, size*2)
}

> invert :: MonadThrow m => Matrix -> m Matrix
> invert m
>     | not (isSquare m) = throwM $ DimensionMismatch
>                                 $ unwords [ "Can't invert non-square matrix of size"
>                                           , show (matrixSize m)
>                                           ]
>     | otherwise = do
>         let size = V.length m
>         let work = identityMatrix size
>         work' <- augment m work
>         work'' <- gaussianElimination work'
>         return $ subMatrix work'' 0 size size (size * 2)

func (m matrix) gaussianElimination() error {
	rows := len(m)
	columns := len(m[0])
	// Clear out the part below the main diagonal and scale the main
	// diagonal to be 1.
	for r := 0; r < rows; r++ {
		// If the element on the diagonal is 0, find a row below
		// that has a non-zero and swap them.
		if m[r][r] == 0 {
			for rowBelow := r + 1; rowBelow < rows; rowBelow++ {
				if m[rowBelow][r] != 0 {
					m.SwapRows(r, rowBelow)
					break
				}
			}
		}
		// If we couldn't find one, the matrix is singular.
		if m[r][r] == 0 {
			return errSingular
		}
		// Scale to 1.
		if m[r][r] != 1 {
			scale := galDivide(1, m[r][r])
			for c := 0; c < columns; c++ {
				m[r][c] = galMultiply(m[r][c], scale)
			}
		}
		// Make everything below the 1 be a 0 by subtracting
		// a multiple of it.  (Subtraction and addition are
		// both exclusive or in the Galois field.)
		for rowBelow := r + 1; rowBelow < rows; rowBelow++ {
			if m[rowBelow][r] != 0 {
				scale := m[rowBelow][r]
				for c := 0; c < columns; c++ {
					m[rowBelow][c] ^= galMultiply(scale, m[r][c])
				}
			}
		}
	}

	// Now clear the part above the main diagonal.
	for d := 0; d < rows; d++ {
		for rowAbove := 0; rowAbove < d; rowAbove++ {
			if m[rowAbove][d] != 0 {
				scale := m[rowAbove][d]
				for c := 0; c < columns; c++ {
					m[rowAbove][c] ^= galMultiply(scale, m[d][c])
				}

			}
		}
	}
	return nil
}

> gaussianElimination :: MonadThrow m => Matrix -> m Matrix
> gaussianElimination = VE.modify $ \m -> do
>     let rows = MV.length m
>
>     numLoop 0 (rows - 1) $ \r -> do
>         mrr <- load m r r
>         when (mrr == 0) $ do
>             break <- lift $ newSTRef False
>             numLoop (r + 1) (rows - 1) $ \rowBelow -> do
>                 doBreak <- lift $ readSTRef break
>                 unless doBreak $ do
>                     mrowBelowr <- load m rowBelow r
>                     when (mrowBelowr /= 0) $ do
>                         lift $ swapRows m r rowBelow
>                         lift $ writeSTRef break True
>
>         mrr' <- load m r r
>         when (mrr' == 0) $ do
>             throwM SingularMatrix
>
>         when (mrr' /= 1) $ do
>             scale <- Galois.galDivide 1 mrr'
>             mr <- VL.read m r
>             let mr' = V.map (\mrc -> Galois.galMultiply mrc scale) mr
>             VL.write m r mr'
>
>         when (rows > r + 1) $ numLoop (r + 1) (rows - 1) $ \rowBelow -> do
>             mrowBelowr <- load m rowBelow r
>             when (mrowBelowr /= 0) $ do
>                 let scale = mrowBelowr
>                 mr <- VL.read m r
>                 mrowBelow <- VL.read m rowBelow
>                 let mrowBelow' = V.zipWith xor mrowBelow (V.map (\mrc -> Galois.galMultiply scale mrc) mr)
>                 VL.write m rowBelow mrowBelow'
>
>     numLoop 0 (rows - 1) $ \d ->
>         when (d > 0) $ numLoop 0 (d - 1) $ \rowAbove -> do
>             mrowAboved <- load m rowAbove d
>             when (mrowAboved /= 0) $ do
>                 let scale = mrowAboved
>                 mrowAbove <- VL.read m rowAbove
>                 md <- VL.read m d
>                 let mrowAbove' = V.zipWith xor mrowAbove (V.map (\mdc -> Galois.galMultiply scale mdc) md)
>                 VL.write m rowAbove mrowAbove'
>   where
>     load :: V.MVector s (SV.Vector Word8) -> Int -> Int -> CatchST s Word8
>     load m row col = do
>         r <- VL.read m row
>         return $ (V.!) r col

// Create a Vandermonde matrix, which is guaranteed to have the
// property that any subset of rows that forms a square matrix
// is invertible.
func vandermonde(rows, cols int) (matrix, error) {
	result, err := newMatrix(rows, cols)
	if err != nil {
		return nil, err
	}
	for r, row := range result {
		for c := range row {
			result[r][c] = galExp(byte(r), c)
		}
	}
	return result, nil
}

> vandermonde :: Int -> Int -> Matrix
> vandermonde rows cols =
>     V.generate rows $ \r ->
>         V.generate cols $ \c ->
>             Galois.galExp (fromIntegral r) c
