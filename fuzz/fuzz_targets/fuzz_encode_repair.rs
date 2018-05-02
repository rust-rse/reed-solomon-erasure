#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate reed_solomon_erasure;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here
});
