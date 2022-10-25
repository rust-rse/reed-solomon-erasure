#![cfg_attr(nightly, feature(test))]

#[cfg(nightly)]
extern crate test;

#[cfg(nightly)]
use {
    rand::{prelude::*, Rng},
    reed_solomon_erasure::galois_8::Field,
    test::Bencher,
};

#[cfg(nightly)]
type ReedSolomon = reed_solomon_erasure::ReedSolomon<Field>;

#[cfg(nightly)]
const SHARD_SIZE: usize = 1024;

#[cfg(nightly)]
fn run_reconstruct_bench(bencher: &mut Bencher, num_data_shards: usize, num_parity_shards: usize) {
    let mut rng = rand::thread_rng();
    let mut shards = vec![vec![0u8; SHARD_SIZE]; num_data_shards + num_parity_shards];
    for shard in &mut shards[..num_data_shards] {
        rng.fill(&mut shard[..]);
    }
    let reed_solomon = ReedSolomon::new(num_data_shards, num_parity_shards).unwrap();
    reed_solomon.encode(&mut shards[..]).unwrap();
    let shards: Vec<_> = shards.into_iter().map(Some).collect();

    bencher.iter(|| {
        let mut shards = shards.clone();
        for _ in 0..num_parity_shards {
            *shards.choose_mut(&mut rng).unwrap() = None;
        }
        reed_solomon.reconstruct(&mut shards[..]).unwrap();
        assert!(shards.iter().all(Option::is_some));
    });
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_2_2(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 2, 2)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_4_2(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 4, 2)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_4_4(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 4, 4)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_8_2(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 8, 2)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_8_4(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 8, 4)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_8_8(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 8, 8)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_16_2(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 16, 2)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_16_4(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 16, 4)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_16_8(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 16, 8)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_16_16(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 16, 16)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_32_2(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 32, 2)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_32_4(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 32, 4)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_32_8(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 32, 8)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_32_16(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 32, 16)
}

#[cfg(nightly)]
#[bench]
fn bench_reconstruct_32_32(bencher: &mut Bencher) {
    run_reconstruct_bench(bencher, 32, 32)
}
