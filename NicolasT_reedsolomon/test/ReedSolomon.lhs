> {-# LANGUAGE OverloadedLists #-}
> module ReedSolomon (tests) where
>
> import Control.Exception.Base (catch, fromException, throwIO, try)
> import Control.Monad (void)
>
> import System.Random (mkStdGen, randoms)
>
> import Test.Tasty (TestTree, testGroup)
> import Test.Tasty.HUnit (Assertion, (@?=), assertBool, assertFailure, testCase)
> import Test.Tasty.QuickCheck (testProperty)
>
> import Test.QuickCheck (Gen, Positive(Positive), arbitrary, choose, vectorOf)
>
> import qualified Data.Vector.Generic as V
>
> import qualified Data.ReedSolomon as RS

/**
 * Unit tests for ReedSolomon
 *
 * Copyright 2015, Klaus Post
 * Copyright 2015, Backblaze, Inc.  All rights reserved.
 */

package reedsolomon

import (
	"bytes"
	"fmt"
	"math/rand"
	"runtime"
	"testing"
)

func TestEncoding(t *testing.T) {
	perShard := 50000
	r, err := New(10, 3)
	if err != nil {
		t.Fatal(err)
	}
	shards := make([][]byte, 13)
	for s := range shards {
		shards[s] = make([]byte, perShard)
	}

	rand.Seed(0)
	for s := 0; s < 13; s++ {
		fillRandom(shards[s])
	}

	err = r.Encode(shards)
	if err != nil {
		t.Fatal(err)
	}
	ok, err := r.Verify(shards)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Fatal("Verification failed")
	}

	err = r.Encode(make([][]byte, 1))
	if err != ErrTooFewShards {
		t.Errorf("expected %v, got %v", ErrTooFewShards, err)
	}

	badShards := make([][]byte, 13)
	badShards[0] = make([]byte, 1)
	err = r.Encode(badShards)
	if err != ErrShardSize {
		t.Errorf("expected %v, got %v", ErrShardSize, err)
	}
}

> testEncoding :: Assertion
> testEncoding = do
>     let perShard = 50000
>     r <- RS.new 10 3
>     let randoms' = randoms $ mkStdGen 0
>         shards = flip (V.unfoldrN 10) randoms' $ \s ->
>                     let (h, t) = splitAt perShard s in
>                     Just (V.fromListN perShard h, t)
>
>     parities <- RS.encode RS.defaultBackend r shards
>     let allChunks = (V.++) shards parities
>
>     assertBool "Verification failed" =<< RS.verify RS.defaultBackend r allChunks
>
>     catch
>         (void $ RS.encode RS.defaultBackend r [[]])
>         (\(RS.InvalidNumberOfShards RS.DataShard 1) -> return ())
>
>     catch
>         (void $ RS.encode RS.defaultBackend r (V.fromListN 13 ([1] : replicate 12 [])))
>         (\(RS.InvalidNumberOfShards RS.DataShard 13) -> return ())

func TestReconstruct(t *testing.T) {
	perShard := 50000
	r, err := New(10, 3)
	if err != nil {
		t.Fatal(err)
	}
	shards := make([][]byte, 13)
	for s := range shards {
		shards[s] = make([]byte, perShard)
	}

	rand.Seed(0)
	for s := 0; s < 13; s++ {
		fillRandom(shards[s])
	}

	err = r.Encode(shards)
	if err != nil {
		t.Fatal(err)
	}

	// Reconstruct with all shards present
	err = r.Reconstruct(shards)
	if err != nil {
		t.Fatal(err)
	}

	// Reconstruct with 10 shards present
	shards[0] = nil
	shards[7] = nil
	shards[11] = nil

	err = r.Reconstruct(shards)
	if err != nil {
		t.Fatal(err)
	}

	ok, err := r.Verify(shards)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Fatal("Verification failed")
	}

	// Reconstruct with 9 shards present (should fail)
	shards[0] = nil
	shards[4] = nil
	shards[7] = nil
	shards[11] = nil

	err = r.Reconstruct(shards)
	if err != ErrTooFewShards {
		t.Errorf("expected %v, got %v", ErrTooFewShards, err)
	}

	err = r.Reconstruct(make([][]byte, 1))
	if err != ErrTooFewShards {
		t.Errorf("expected %v, got %v", ErrTooFewShards, err)
	}
	err = r.Reconstruct(make([][]byte, 13))
	if err != ErrShardNoData {
		t.Errorf("expected %v, got %v", ErrShardNoData, err)
	}
}

> testReconstruct :: Assertion
> testReconstruct = do
>     let perShard = 50000
>     r <- RS.new 10 3
>     let randoms' = randoms $ mkStdGen 0
>         shards = flip (V.unfoldrN 10) randoms' $ \s ->
>                     let (h, t) = splitAt perShard s in
>                     Just (V.fromListN perShard h, t)
>     parities <- RS.encode RS.defaultBackend r shards
>     let toReconstruct = V.map Just ((V.++) shards parities)
>     all' <- RS.reconstruct RS.defaultBackend r toReconstruct
>
>     all' @?= (V.++) shards parities
>
>     shards' <- RS.reconstruct RS.defaultBackend r $ (V.//) toReconstruct [ (0, Nothing)
>                                                                          , (7, Nothing)
>                                                                          , (11, Nothing)
>                                                                          ]
>     verified <- RS.verify RS.defaultBackend r shards'
>     verified @?= True
>
>     catch
>         (do
>             _ <- RS.reconstruct RS.defaultBackend r $ (V.//) toReconstruct [ (0, Nothing)
>                                                                            , (4, Nothing)
>                                                                            , (7, Nothing)
>                                                                            , (11, Nothing)
>                                                                            ]
>             assertFailure "Expected 'Too few shards'")
>         (\(RS.InvalidNumberOfShards RS.AnyShard 9) -> return ())
>
>     catch
>         (do
>             _ <- RS.reconstruct RS.defaultBackend r [Just []]
>             assertFailure "Expected 'Too few shards'")
>         (\(RS.InvalidNumberOfShards RS.AnyShard 1) -> return ())
>
>     catch
>         (do
>             _ <- RS.reconstruct RS.defaultBackend r (V.replicate 13 Nothing)
>             assertFailure "Expected 'No shard data'")
>         (\RS.EmptyShards -> return ())

func TestVerify(t *testing.T) {
	perShard := 33333
	r, err := New(10, 4)
	if err != nil {
		t.Fatal(err)
	}
	shards := make([][]byte, 14)
	for s := range shards {
		shards[s] = make([]byte, perShard)
	}

	rand.Seed(0)
	for s := 0; s < 10; s++ {
		fillRandom(shards[s])
	}

	err = r.Encode(shards)
	if err != nil {
		t.Fatal(err)
	}
	ok, err := r.Verify(shards)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Fatal("Verification failed")
	}

	// Put in random data. Verification should fail
	fillRandom(shards[10])
	ok, err = r.Verify(shards)
	if err != nil {
		t.Fatal(err)
	}
	if ok {
		t.Fatal("Verification did not fail")
	}
	// Re-encode
	err = r.Encode(shards)
	if err != nil {
		t.Fatal(err)
	}
	// Fill a data segment with random data
	fillRandom(shards[0])
	ok, err = r.Verify(shards)
	if err != nil {
		t.Fatal(err)
	}
	if ok {
		t.Fatal("Verification did not fail")
	}

	_, err = r.Verify(make([][]byte, 1))
	if err != ErrTooFewShards {
		t.Errorf("expected %v, got %v", ErrTooFewShards, err)
	}

	_, err = r.Verify(make([][]byte, 14))
	if err != ErrShardNoData {
		t.Errorf("expected %v, got %v", ErrShardNoData, err)
	}
}

> testVerify :: Assertion
> testVerify = do
>     let perShard = 33333
>     r <- RS.new 10 4
>     let randoms' = randoms $ mkStdGen 0
>         dataShards = flip (V.unfoldrN 10) randoms' $ \s ->
>                         let (h, t) = splitAt perShard s in
>                         Just (V.fromListN perShard h, t)
>
>     parityShards <- RS.encode RS.defaultBackend r dataShards
>     let shards = (V.++) dataShards parityShards
>
>     assertBool "Verification failed" =<< RS.verify RS.defaultBackend r shards
>
>     let shards' = V.update shards [(10, V.replicate perShard 0)]
>
>     assertBool "Verification did not fail" =<< not `fmap` RS.verify RS.defaultBackend r shards'
>
>     let shards'' = V.update shards [(0, V.replicate perShard 0)]
>
>     assertBool "Verification did not fail" =<< not `fmap` RS.verify RS.defaultBackend r shards''
>
>     catch
>         (void $ RS.verify RS.defaultBackend r [[]])
>         (\(RS.InvalidNumberOfShards RS.DataShard 1) -> return ())
>
>     catch
>         (void $ RS.verify RS.defaultBackend r (V.replicate 14 []))
>         (\RS.EmptyShards -> return ())

func TestOneEncode(t *testing.T) {
	codec, err := New(5, 5)
	if err != nil {
		t.Fatal(err)
	}
	shards := [][]byte{
		{0, 1},
		{4, 5},
		{2, 3},
		{6, 7},
		{8, 9},
		{0, 0},
		{0, 0},
		{0, 0},
		{0, 0},
		{0, 0},
	}
	codec.Encode(shards)
	if shards[5][0] != 12 || shards[5][1] != 13 {
		t.Fatal("shard 5 mismatch")
	}
	if shards[6][0] != 10 || shards[6][1] != 11 {
		t.Fatal("shard 6 mismatch")
	}
	if shards[7][0] != 14 || shards[7][1] != 15 {
		t.Fatal("shard 7 mismatch")
	}
	if shards[8][0] != 90 || shards[8][1] != 91 {
		t.Fatal("shard 8 mismatch")
	}
	if shards[9][0] != 94 || shards[9][1] != 95 {
		t.Fatal("shard 9 mismatch")
	}

	ok, err := codec.Verify(shards)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Fatal("did not verify")
	}
	shards[8][0]++
	ok, err = codec.Verify(shards)
	if err != nil {
		t.Fatal(err)
	}
	if ok {
		t.Fatal("verify did not fail as expected")
	}

}

> testOneEncode :: Assertion
> testOneEncode = do
>     codec <- RS.new 5 5
>     let shards = [ [0, 1]
>                  , [4, 5]
>                  , [2, 3]
>                  , [6, 7]
>                  , [8, 9]
>                  ]
>     parity <- RS.encode RS.defaultBackend codec shards
>     let expected = [ [12, 13]
>                    , [10, 11]
>                    , [14, 15]
>                    , [90, 91]
>                    , [94, 95]
>                    ]
>     parity @?= expected
>
>     let allShards = (V.++) shards expected
>
>     verified <- RS.verify RS.defaultBackend codec allShards
>     verified @?= True
>
>     let allShards' = (V.//) allShards [(8, [91, 91])]
>
>     verified' <- RS.verify RS.defaultBackend codec allShards'
>     verified' @?= False

func fillRandom(b []byte) {
	for i := range b {
		b[i] = byte(rand.Int() & 0xff)
	}
}

func benchmarkEncode(b *testing.B, dataShards, parityShards, shardSize int) {
	r, err := New(dataShards, parityShards)
	if err != nil {
		b.Fatal(err)
	}
	shards := make([][]byte, dataShards+parityShards)
	for s := range shards {
		shards[s] = make([]byte, shardSize)
	}

	rand.Seed(0)
	for s := 0; s < dataShards; s++ {
		fillRandom(shards[s])
	}

	b.SetBytes(int64(shardSize * dataShards))
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		err = r.Encode(shards)
		if err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkEncode10x2x10000(b *testing.B) {
	benchmarkEncode(b, 10, 2, 10000)
}

func BenchmarkEncode100x20x10000(b *testing.B) {
	benchmarkEncode(b, 100, 20, 10000)
}

func BenchmarkEncode17x3x1M(b *testing.B) {
	benchmarkEncode(b, 17, 3, 1024*1024)
}

// Benchmark 10 data shards and 4 parity shards with 16MB each.
func BenchmarkEncode10x4x16M(b *testing.B) {
	benchmarkEncode(b, 10, 4, 16*1024*1024)
}

// Benchmark 5 data shards and 2 parity shards with 1MB each.
func BenchmarkEncode5x2x1M(b *testing.B) {
	benchmarkEncode(b, 5, 2, 1024*1024)
}

// Benchmark 1 data shards and 2 parity shards with 1MB each.
func BenchmarkEncode10x2x1M(b *testing.B) {
	benchmarkEncode(b, 10, 2, 1024*1024)
}

// Benchmark 10 data shards and 4 parity shards with 1MB each.
func BenchmarkEncode10x4x1M(b *testing.B) {
	benchmarkEncode(b, 10, 4, 1024*1024)
}

// Benchmark 50 data shards and 20 parity shards with 1MB each.
func BenchmarkEncode50x20x1M(b *testing.B) {
	benchmarkEncode(b, 50, 20, 1024*1024)
}

// Benchmark 17 data shards and 3 parity shards with 16MB each.
func BenchmarkEncode17x3x16M(b *testing.B) {
	benchmarkEncode(b, 17, 3, 16*1024*1024)
}

func benchmarkVerify(b *testing.B, dataShards, parityShards, shardSize int) {
	r, err := New(dataShards, parityShards)
	if err != nil {
		b.Fatal(err)
	}
	shards := make([][]byte, parityShards+dataShards)
	for s := range shards {
		shards[s] = make([]byte, shardSize)
	}

	rand.Seed(0)
	for s := 0; s < dataShards; s++ {
		fillRandom(shards[s])
	}
	err = r.Encode(shards)
	if err != nil {
		b.Fatal(err)
	}

	b.SetBytes(int64(shardSize * dataShards))
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err = r.Verify(shards)
		if err != nil {
			b.Fatal(err)
		}
	}
}

// Benchmark 10 data slices with 2 parity slices holding 10000 bytes each
func BenchmarkVerify10x2x10000(b *testing.B) {
	benchmarkVerify(b, 10, 2, 10000)
}

// Benchmark 50 data slices with 5 parity slices holding 100000 bytes each
func BenchmarkVerify50x5x50000(b *testing.B) {
	benchmarkVerify(b, 50, 5, 100000)
}

// Benchmark 10 data slices with 2 parity slices holding 1MB bytes each
func BenchmarkVerify10x2x1M(b *testing.B) {
	benchmarkVerify(b, 10, 2, 1024*1024)
}

// Benchmark 5 data slices with 2 parity slices holding 1MB bytes each
func BenchmarkVerify5x2x1M(b *testing.B) {
	benchmarkVerify(b, 5, 2, 1024*1024)
}

// Benchmark 10 data slices with 4 parity slices holding 1MB bytes each
func BenchmarkVerify10x4x1M(b *testing.B) {
	benchmarkVerify(b, 10, 4, 1024*1024)
}

// Benchmark 5 data slices with 2 parity slices holding 1MB bytes each
func BenchmarkVerify50x20x1M(b *testing.B) {
	benchmarkVerify(b, 50, 20, 1024*1024)
}

// Benchmark 10 data slices with 4 parity slices holding 16MB bytes each
func BenchmarkVerify10x4x16M(b *testing.B) {
	benchmarkVerify(b, 10, 4, 16*1024*1024)
}

// Simple example of how to use all functions of the Encoder.
// Note that all error checks have been removed to keep it short.
func ExampleEncoder() {
	// Create some sample data
	var data = make([]byte, 250000)
	fillRandom(data)

	// Create an encoder with 17 data and 3 parity slices.
	enc, _ := New(17, 3)

	// Split the data into shards
	shards, _ := enc.Split(data)

	// Encode the parity set
	_ = enc.Encode(shards)

	// Verify the parity set
	ok, _ := enc.Verify(shards)
	if ok {
		fmt.Println("ok")
	}

	// Delete two shards
	shards[10], shards[11] = nil, nil

	// Reconstruct the shards
	_ = enc.Reconstruct(shards)

	// Verify the data set
	ok, _ = enc.Verify(shards)
	if ok {
		fmt.Println("ok")
	}
	// Output: ok
	// ok
}

// This demonstrates that shards can be arbitrary sliced and
// merged and still remain valid.
func ExampleEncoder_slicing() {
	// Create some sample data
	var data = make([]byte, 250000)
	fillRandom(data)

	// Create 5 data slices of 50000 elements each
	enc, _ := New(5, 3)
	shards, _ := enc.Split(data)
	err := enc.Encode(shards)
	if err != nil {
		panic(err)
	}

	// Check that it verifies
	ok, err := enc.Verify(shards)
	if ok && err == nil {
		fmt.Println("encode ok")
	}

	// Split the data set of 50000 elements into two of 25000
	splitA := make([][]byte, 8)
	splitB := make([][]byte, 8)

	// Merge into a 100000 element set
	merged := make([][]byte, 8)

	// Split/merge the shards
	for i := range shards {
		splitA[i] = shards[i][:25000]
		splitB[i] = shards[i][25000:]

		// Concencate it to itself
		merged[i] = append(make([]byte, 0, len(shards[i])*2), shards[i]...)
		merged[i] = append(merged[i], shards[i]...)
	}

	// Each part should still verify as ok.
	ok, err = enc.Verify(shards)
	if ok && err == nil {
		fmt.Println("splitA ok")
	}

	ok, err = enc.Verify(splitB)
	if ok && err == nil {
		fmt.Println("splitB ok")
	}

	ok, err = enc.Verify(merged)
	if ok && err == nil {
		fmt.Println("merge ok")
	}
	// Output: encode ok
	// splitA ok
	// splitB ok
	// merge ok
}

// This demonstrates that shards can xor'ed and
// still remain a valid set.
//
// The xor value must be the same for element 'n' in each shard,
// except if you xor with a similar sized encoded shard set.
func ExampleEncoder_xor() {
	// Create some sample data
	var data = make([]byte, 25000)
	fillRandom(data)

	// Create 5 data slices of 5000 elements each
	enc, _ := New(5, 3)
	shards, _ := enc.Split(data)
	err := enc.Encode(shards)
	if err != nil {
		panic(err)
	}

	// Check that it verifies
	ok, err := enc.Verify(shards)
	if !ok || err != nil {
		fmt.Println("falied initial verify", err)
	}

	// Create an xor'ed set
	xored := make([][]byte, 8)

	// We xor by the index, so you can see that the xor can change,
	// It should however be constant vertically through your slices.
	for i := range shards {
		xored[i] = make([]byte, len(shards[i]))
		for j := range xored[i] {
			xored[i][j] = shards[i][j] ^ byte(j&0xff)
		}
	}

	// Each part should still verify as ok.
	ok, err = enc.Verify(xored)
	if ok && err == nil {
		fmt.Println("verified ok after xor")
	}
	// Output: verified ok after xor
}

func TestEncoderReconstruct(t *testing.T) {
	// Create some sample data
	var data = make([]byte, 250000)
	fillRandom(data)

	// Create 5 data slices of 50000 elements each
	enc, _ := New(5, 3)
	shards, _ := enc.Split(data)
	err := enc.Encode(shards)
	if err != nil {
		t.Fatal(err)
	}

	// Check that it verifies
	ok, err := enc.Verify(shards)
	if !ok || err != nil {
		t.Fatal("not ok:", ok, "err:", err)
	}

	// Delete a shard
	shards[0] = nil

	// Should reconstruct
	err = enc.Reconstruct(shards)
	if err != nil {
		t.Fatal(err)
	}

	// Check that it verifies
	ok, err = enc.Verify(shards)
	if !ok || err != nil {
		t.Fatal("not ok:", ok, "err:", err)
	}

	// Recover original bytes
	buf := new(bytes.Buffer)
	err = enc.Join(buf, shards, len(data))
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(buf.Bytes(), data) {
		t.Fatal("recovered bytes do not match")
	}

	// Corrupt a shard
	shards[0] = nil
	shards[1][0], shards[1][500] = 75, 75

	// Should reconstruct (but with corrupted data)
	err = enc.Reconstruct(shards)
	if err != nil {
		t.Fatal(err)
	}

	// Check that it verifies
	ok, err = enc.Verify(shards)
	if ok || err != nil {
		t.Fatal("error or ok:", ok, "err:", err)
	}

	// Recovered data should not match original
	buf.Reset()
	err = enc.Join(buf, shards, len(data))
	if err != nil {
		t.Fatal(err)
	}
	if bytes.Equal(buf.Bytes(), data) {
		t.Fatal("corrupted data matches original")
	}
}

func TestSplitJoin(t *testing.T) {
	var data = make([]byte, 250000)
	rand.Seed(0)
	fillRandom(data)

	enc, _ := New(5, 3)
	shards, err := enc.Split(data)
	if err != nil {
		t.Fatal(err)
	}

	_, err = enc.Split([]byte{})
	if err != ErrShortData {
		t.Errorf("expected %v, got %v", ErrShortData, err)
	}

	buf := new(bytes.Buffer)
	err = enc.Join(buf, shards, 50)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(buf.Bytes(), data[:50]) {
		t.Fatal("recovered data does match original")
	}

	err = enc.Join(buf, [][]byte{}, 0)
	if err != ErrTooFewShards {
		t.Errorf("expected %v, got %v", ErrTooFewShards, err)
	}

	err = enc.Join(buf, shards, len(data)+1)
	if err != ErrShortData {
		t.Errorf("expected %v, got %v", ErrShortData, err)
	}
}

> testSplitJoin :: Assertion
> testSplitJoin = do
>     let dataSize = 250000
>         data_ = V.fromListN dataSize $ randoms $ mkStdGen 0
>
>     let Right enc = RS.new 5 3
>     shards <- RS.split enc data_
>
>     let Left err = RS.split enc V.empty
>     fromException err @?= Just RS.InvalidDataSize
>
>     buf <- RS.join enc shards 50
>     buf @?= V.take 50 data_
>
>     let Left err' = RS.join enc V.empty 0
>     fromException err' @?= Just (RS.InvalidNumberOfShards RS.DataShard 0)
>
>     let Left err'' = RS.join enc shards (dataSize + 1)
>     fromException err'' @?= Just RS.InvalidDataSize

func TestCodeSomeShards(t *testing.T) {
	var data = make([]byte, 250000)
	fillRandom(data)
	enc, _ := New(5, 3)
	r := enc.(*reedSolomon) // need to access private methods
	shards, _ := enc.Split(data)

	old := runtime.GOMAXPROCS(1)
	r.codeSomeShards(r.parity, shards[:r.DataShards], shards[r.DataShards:], r.ParityShards, len(shards[0]))

	// hopefully more than 1 CPU
	runtime.GOMAXPROCS(runtime.NumCPU())
	r.codeSomeShards(r.parity, shards[:r.DataShards], shards[r.DataShards:], r.ParityShards, len(shards[0]))

	// reset MAXPROCS, otherwise testing complains
	runtime.GOMAXPROCS(old)
}

func TestAllMatrices(t *testing.T) {
	t.Skip("Skipping slow matrix check")
	for i := 1; i < 257; i++ {
		_, err := New(i, i)
		if err != nil {
			t.Fatal("creating matrix size", i, i, ":", err)
		}
	}
}

func TestNew(t *testing.T) {
	tests := []struct {
		data, parity int
		err          error
	}{
		{10, 500, nil},
		{256, 256, nil},

		{0, 1, ErrInvShardNum},
		{1, 0, ErrInvShardNum},
		{257, 1, ErrInvShardNum},

		// overflow causes r.Shards to be negative
		{256, int(^uint(0) >> 1), errInvalidRowSize},
	}
	for _, test := range tests {
		_, err := New(test.data, test.parity)
		if err != test.err {
			t.Errorf("New(%v, %v): expected %v, got %v", test.data, test.parity, test.err, err)
		}
	}
}

> testNew :: Assertion
> testNew = do
>     let tests' = [ (10, 500, Nothing)
>                  , (256, 256, Nothing)
>                  , (0, 1, Just $ RS.InvalidNumberOfShards RS.DataShard 0)
>                  , (1, 0, Just $ RS.InvalidNumberOfShards RS.ParityShard 0)
>                  , (257, 1, Just $ RS.InvalidNumberOfShards RS.DataShard 257)
>                  -- , (256, maxBound `shiftR` 1, Just "Invalid number of shards")
>                  ]  :: [(Int, Int, Maybe RS.ValueError)]
>
>     mapM_ (\(data_, parity, err) -> do
>         r <- try $ RS.new data_ parity
>         case err of
>             Nothing ->
>                 case r of
>                     Right _ -> return ()
>                     Left e -> throwIO e
>             Just e ->
>                 case r of
>                     Right _ -> assertFailure "No exception thrown"
>                     Left e' ->
>                         case fromException e' of
>                             Nothing -> throwIO e'
>                             Just m -> m @?= e)
>         tests'

> encodeRecover :: Positive Int -> Gen Bool
> encodeRecover (Positive fragmentSize) = do
>     numDataShards <- choose (1, 10)
>     numParities <- choose (1, 10)
>
>     let Right r = RS.new numDataShards numParities
>
>     dataShards <- V.replicateM numDataShards (V.replicateM fragmentSize arbitrary)
>     let Just parities = RS.encode RS.defaultBackend r dataShards
>         allFragments = (V.++) dataShards parities
>
>     dropped <- do
>         numDrops <- choose (0, numParities)
>         let dropIdx = choose (0, numDataShards + numParities - 1)
>         vectorOf numDrops dropIdx
>
>     let someFragments = (V.//) (V.map Just allFragments) $ map (\idx -> (idx, Nothing)) dropped
>         Right recovered = RS.reconstruct RS.defaultBackend r someFragments
>
>     return $ recovered == allFragments

> splitJoin :: Positive Int -> Gen Bool
> splitJoin (Positive len) = do
>     numDataShards <- choose (1, 10)
>     let numParityShards = 2
>
>     let Right r = RS.new numDataShards numParityShards
>     vec <- V.replicateM len arbitrary
>
>     case RS.split r vec of
>         Left e ->
>             return $
>                 if len < numDataShards
>                 then fromException e == Just RS.InvalidDataSize
>                 else False
>         Right parts ->
>             let Right joined = RS.join r parts len in
>             return $ joined == vec

> tests :: TestTree
> tests = testGroup "ReedSolomon" [
>       testGroup "Unit" [
>             testCase "testEncoding" testEncoding
>           , testCase "testReconstruct" testReconstruct
>           , testCase "testVerify" testVerify
>           , testCase "testOneEncode" testOneEncode
>           , testCase "testNew" testNew
>           , testCase "testSplitJoin" testSplitJoin
>           ]
>     , testGroup "Properties" [
>             testProperty "encodeRecover" encodeRecover
>           , testProperty "splitJoin" splitJoin
>           ]
>     ]
