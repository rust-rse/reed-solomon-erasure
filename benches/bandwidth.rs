use std::convert::TryInto;

use criterion::measurement::WallTime;
use criterion::{black_box, criterion_group, criterion_main, BenchmarkGroup, Criterion};
use rand::distributions::{Distribution, Standard};
use rand::rngs::SmallRng;
use rand::SeedableRng;
use reed_solomon_erasure::galois_8::ReedSolomon;

type Shards = Vec<Vec<u8>>;

fn reed_solomon(shards: &mut Shards, data: usize, parity: usize) {
    let r = ReedSolomon::new(data, parity).unwrap(); // 3 data shards, 2 parity shards

    // Construct the parity shards
    r.encode(shards).unwrap();
}

fn create_dataset(block_size: usize, data: usize, parity: usize) -> Shards {
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

fn rs_benchmark(
    group: &mut BenchmarkGroup<WallTime>,
    block_size: usize,
    data_shards: usize,
    parity_shards: usize,
) {
    let size = block_size * data_shards;

    group.throughput(criterion::Throughput::Bytes(size.try_into().unwrap()));

    group.bench_function(format!("Encode {}+{}", data_shards, parity_shards), |b| {
        let mut shards = create_dataset(block_size, data_shards, parity_shards);

        assert_eq!(shards.len(), data_shards + parity_shards);
        assert_eq!(shards.last().unwrap().len(), block_size);
        
        b.iter(|| reed_solomon(black_box(&mut shards), data_shards, parity_shards))
    });
}

fn criterion_benchmark(c: &mut Criterion) {
    // let plot_config = PlotConfiguration::default();
    {
        let mut group = c.benchmark_group("Galos 8 [1024KB]");
        // group.plot_config(plot_config.clone());
        rs_benchmark(&mut group, 1024, 4, 4);
        rs_benchmark(&mut group, 1024, 8, 8);
        rs_benchmark(&mut group, 1024, 16, 16);
        rs_benchmark(&mut group, 1024, 32, 32);
        rs_benchmark(&mut group, 1024, 64, 64);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [2048KB]");
        // group.plot_config(plot_config.clone());
        rs_benchmark(&mut group, 2048, 4, 4);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [4096KB]");
        // group.plot_config(plot_config.clone());
        rs_benchmark(&mut group, 4096, 4, 4);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [8192KB]");
        // group.plot_config(plot_config.clone());
        rs_benchmark(&mut group, 8192, 4, 4);
    }
    {
        let mut group = c.benchmark_group("Galos 8 [16384KB]");
        // group.plot_config(plot_config.clone());
        rs_benchmark(&mut group, 16384, 4, 4);
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
