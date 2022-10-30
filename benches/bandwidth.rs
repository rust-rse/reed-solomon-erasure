use std::convert::TryInto;

use criterion::measurement::WallTime;
use criterion::{black_box, criterion_group, criterion_main, BenchmarkGroup, Criterion};
use rand::distributions::{Distribution, Standard};
use rand::rngs::SmallRng;
use rand::SeedableRng;
use reed_solomon_erasure::galois_8::ReedSolomon;

type Shards = Vec<Vec<u8>>;

fn create_shards(block_size: usize, data: usize, parity: usize) -> Shards {
    let mut small_rng = SmallRng::from_entropy();

    let mut shards = Vec::new();

    // Create data shards with random data
    shards.resize_with(data, || {
        Standard
            .sample_iter(&mut small_rng)
            .take(block_size)
            .collect()
    });

    // Create empty parity shards
    shards.resize_with(data + parity, || {
        let mut vec = Vec::with_capacity(block_size);
        vec.resize(block_size, 0);
        vec
    });

    shards
}

fn rs_encode_benchmark(
    group: &mut BenchmarkGroup<WallTime>,
    block_size: usize,
    data_shards: usize,
    parity_shards: usize,
) {
    let size = block_size * data_shards;

    group.throughput(criterion::Throughput::Bytes(size.try_into().unwrap()));

    group.bench_function(format!("{}+{}", data_shards, parity_shards), |b| {
        let mut shards = create_shards(block_size, data_shards, parity_shards);
        let rs = ReedSolomon::new(data_shards, parity_shards).unwrap();

        assert_eq!(shards.len(), data_shards + parity_shards);
        assert_eq!(shards.last().unwrap().len(), block_size);

        b.iter(|| {
            rs.encode(black_box(&mut shards)).unwrap();
        });
    });
}

fn rs_reconstruct_benchmark(
    group: &mut BenchmarkGroup<WallTime>,
    block_size: usize,
    data_shards: usize,
    parity_shards: usize,
    delete: usize,
) {
    let size = block_size * data_shards;

    group.throughput(criterion::Throughput::Bytes(size.try_into().unwrap()));

    group.bench_function(format!("{}+{}", data_shards, parity_shards), |b| {
        let mut shards = create_shards(block_size, data_shards, parity_shards);
        let rs = ReedSolomon::new(data_shards, parity_shards).unwrap();

        assert_eq!(shards.len(), data_shards + parity_shards);
        assert_eq!(shards.last().unwrap().len(), block_size);

        // Construct the parity shards
        rs.encode(&mut shards).unwrap();

        let mut calculated: Vec<Option<Vec<u8>>> = shards.into_iter().map(Some).collect();

        b.iter(|| {
            (0..delete).for_each(|i| calculated[i] = None);
            rs.reconstruct(black_box(&mut calculated)).unwrap();
        });
    });
}

fn encode(c: &mut Criterion) {
    {
        let mut group = c.benchmark_group("Galos 8 [1KiB] Encode");
        rs_encode_benchmark(&mut group, 1024, 4, 4);
        rs_encode_benchmark(&mut group, 1024, 8, 8);
        rs_encode_benchmark(&mut group, 1024, 16, 16);
        rs_encode_benchmark(&mut group, 1024, 32, 32);
        rs_encode_benchmark(&mut group, 1024, 64, 64);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [2KiB] Encode");
        rs_encode_benchmark(&mut group, 2048, 4, 4);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [4KiB] Encode");
        rs_encode_benchmark(&mut group, 4096, 4, 4);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [8KiB] Encode");
        rs_encode_benchmark(&mut group, 8192, 4, 4);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [16KiB] Encode");
        rs_encode_benchmark(&mut group, 16384, 4, 4);
    }
}

fn reconstruct_one(c: &mut Criterion) {
    {
        let mut group = c.benchmark_group("Galos 8 [1KiB] Reconstruct One");
        rs_reconstruct_benchmark(&mut group, 1024, 4, 4, 1);
        rs_reconstruct_benchmark(&mut group, 1024, 8, 8, 1);
        rs_reconstruct_benchmark(&mut group, 1024, 16, 16, 1);
        rs_reconstruct_benchmark(&mut group, 1024, 32, 32, 1);
        rs_reconstruct_benchmark(&mut group, 1024, 64, 64, 1);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [2KiB] Reconstruct One");
        rs_reconstruct_benchmark(&mut group, 2048, 4, 4, 1);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [4KiB] Reconstruct One");
        rs_reconstruct_benchmark(&mut group, 4096, 4, 4, 1);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [8KiB] Reconstruct One");
        rs_reconstruct_benchmark(&mut group, 8192, 4, 4, 1);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [16KiB] Reconstruct One");
        rs_reconstruct_benchmark(&mut group, 16384, 4, 4, 1);
    }
}

fn reconstruct_all(c: &mut Criterion) {
    {
        let mut group = c.benchmark_group("Galos 8 [1KiB] Reconstruct All");
        rs_reconstruct_benchmark(&mut group, 1024, 4, 4, 4);
        rs_reconstruct_benchmark(&mut group, 1024, 8, 8, 8);
        rs_reconstruct_benchmark(&mut group, 1024, 16, 16, 16);
        rs_reconstruct_benchmark(&mut group, 1024, 32, 32, 32);
        rs_reconstruct_benchmark(&mut group, 1024, 64, 64, 64);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [2KiB] Reconstruct All");
        rs_reconstruct_benchmark(&mut group, 2048, 4, 4, 4);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [4KiB] Reconstruct All");
        rs_reconstruct_benchmark(&mut group, 4096, 4, 4, 4);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [8KiB] Reconstruct All");
        rs_reconstruct_benchmark(&mut group, 8192, 4, 4, 4);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [16KiB] Reconstruct All");
        rs_reconstruct_benchmark(&mut group, 16384, 4, 4, 4);
    }
}

criterion_group!(benches, encode, reconstruct_one, reconstruct_all);
criterion_main!(benches);
