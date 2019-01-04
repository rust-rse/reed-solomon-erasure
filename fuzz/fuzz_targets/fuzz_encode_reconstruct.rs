#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate reed_solomon_erasure;

use reed_solomon_erasure::ReedSolomon;

fuzz_target!(|data: &[u8]| {
    if data.len() >= 7 {
        let data_shards = data[0] as usize;
        let parity_shards = data[1] as usize;
        let shard_size= data[2] as usize;
        let run_count = data[3] as usize;
        let interval= data[4] as usize;
        let corrupt_count = data[5] as usize;
        let corrupt_index = data[6] as usize;

        let data = &data[7..];

        if data_shards > 0
            && parity_shards > 0
            && shard_size > 0
            && interval > 0
            && corrupt_count <= parity_shards
            && data_shards + parity_shards <= 256
            && data.len() == data_shards * shard_size
        {
            let codec = ReedSolomon::new(data_shards, parity_shards).unwrap();

            for _ in 0..run_count {
                assert_eq!(codec.data_shard_count(), data_shards);
                assert_eq!(codec.parity_shard_count(), parity_shards);
                assert_eq!(codec.total_shard_count(), data_shards + parity_shards);

                let mut slice_present: Vec<bool>= vec![true; data_shards + parity_shards];
                let mut data_buffer: Vec<u8>= vec![0u8; data_shards * shard_size];
                data_buffer.copy_from_slice(data);
                let mut data = data_buffer;
                let mut parity_buffer: Vec<u8>= vec![0u8; shard_size * parity_shards];
                {
                    let data_slices: Vec<&[u8]> = data.chunks(shard_size).collect();
                    let mut parity_slices: Vec<&mut [u8]> = parity_buffer.chunks_mut(shard_size).collect();

                    codec.encode_sep(&data_slices, &mut parity_slices).unwrap();
                }

                {
                    let mut data_slices: Vec<&mut [u8]> =
                        data.chunks_mut(shard_size).collect();
                    let mut parity_slices: Vec<&mut [u8]> =
                        parity_buffer.chunks_mut(shard_size).collect();

                    let mut slices: Vec<&mut [u8]> =
                        Vec::with_capacity(data_shards + parity_shards);
                    for d in data_slices.iter_mut() {
                        slices.push(d);
                    }
                    for p in parity_slices.iter_mut() {
                        slices.push(p);
                    }

                    for i in 0..corrupt_count {
                        let corrupt =
                            (corrupt_index + i * interval) % (data_shards + parity_shards);
                        slice_present[corrupt] = false;

                        for i in 0..shard_size {
                            slices[corrupt][i] = 0;
                        }
                    }
                }

                if corrupt_count > 0 {
                    let data_slices: Vec<&[u8]> = data.chunks(shard_size).collect();
                    let parity_slices: Vec<&[u8]> = parity_buffer.chunks(shard_size).collect();

                    let mut slices = Vec::with_capacity(data_shards + parity_shards);
                    for &d in data_slices.iter() {
                        slices.push(d);
                    }
                    for &p in parity_slices.iter() {
                        slices.push(p);
                    }

                    assert!(!codec.verify(&slices).unwrap());
                }

                {
                    let mut data_slices: Vec<&mut [u8]> =
                        data.chunks_mut(shard_size).collect();
                    let mut parity_slices: Vec<&mut [u8]> =
                        parity_buffer.chunks_mut(shard_size).collect();

                    let mut slices: Vec<&mut [u8]> =
                        Vec::with_capacity(data_shards + parity_shards);
                    for d in data_slices.iter_mut() {
                        slices.push(d);
                    }
                    for p in parity_slices.iter_mut() {
                        slices.push(p);
                    }

                    codec.reconstruct(&mut slices,
                                      &mut slice_present).unwrap();
                }

                {
                    let data_slices: Vec<&[u8]> = data.chunks(shard_size).collect();
                    let parity_slices: Vec<&[u8]> = parity_buffer.chunks(shard_size).collect();

                    let mut slices = Vec::with_capacity(data_shards + parity_shards);
                    for &d in data_slices.iter() {
                        slices.push(d);
                    }
                    for &p in parity_slices.iter() {
                        slices.push(p);
                    }

                    assert!(codec.verify(&slices).unwrap());
                }
            }
        }
    }
});
