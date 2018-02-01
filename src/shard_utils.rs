use super::Shard;

/// Makes shard with byte array of zero length.
pub fn make_zero_len_shard() -> Shard {
    Box::new([])
}

pub fn make_zero_len_shards(count : usize) -> Vec<Shard> {
    let mut result = Vec::with_capacity(count);
    for _ in 0..count {
        result.push(make_zero_len_shard());
    }
    result
}

/// Makes shard with byte array filled with zeros of some length.
pub fn make_blank_shard(size : usize) -> Shard {
    vec![0; size].into_boxed_slice()
}

pub fn make_blank_shards(size : usize, count : usize) -> Vec<Shard> {
    let mut result = Vec::with_capacity(count);
    for _ in 0..count {
        result.push(make_blank_shard(size));
    }
    result
}

/// Transforms vector of shards to vector of option shards.
///
/// # Remarks
///
/// Each shard is cloned rather than moved, which may be slow.
///
/// This is mainly useful when you want to repair a vector
/// of shards using `decode_missing`.
pub fn shards_to_option_shards(shards : &[Shard])
                               -> Vec<Option<Shard>> {
    let mut result = Vec::with_capacity(shards.len());

    for v in shards.iter() {
        let inner : Shard = v.clone();
        result.push(Some(inner));
    }
    result
}

/// Transforms vector of shards into vector of option shards.
///
/// # Remarks
///
/// Each shard is moved rather than cloned.
///
/// This is mainly useful when you want to repair a vector
/// of shards using `decode_missing`.
pub fn shards_into_option_shards(shards : Vec<Shard>)
                                 -> Vec<Option<Shard>> {
    let mut result = Vec::with_capacity(shards.len());

    for v in shards.into_iter() {
        result.push(Some(v));
    }
    result
}

/// Transforms a section of vector of option shards to vector of shards.
///
/// # Arguments
///
/// * `start` - start of range of option shards you want to use
/// * `count` - number of option shards you want to use
///
/// # Remarks
///
/// Each shard is cloned rather than moved, which may be slow.
///
/// This is mainly useful when you want to convert result of
/// `decode_missing` to the more usable arrangement.
///
/// Panics when any of the shards is missing.
pub fn option_shards_to_shards(shards : &[Option<Shard>])
                               -> Vec<Shard> {
    let mut result = Vec::with_capacity(shards.len());

    for i in 0..shards.len() {
        let shard = match shards[i] {
            Some(ref x) => x,
            None        => panic!("Missing shard, index : {}", i),
        };
        let inner : Box<[u8]> = shard.clone();
        result.push(inner);
    }
    result
}

/// Transforms vector of option shards into vector of shards.
///
/// # Remarks
///
/// Each shard is moved rather than cloned.
///
/// This is mainly useful when you want to convert result of
/// `decode_missing` to the more usable arrangement.
///
/// Panics when any of the shards is missing.
pub fn option_shards_into_shards(shards : Vec<Option<Shard>>)
                                 -> Vec<Shard> {
    let mut result = Vec::with_capacity(shards.len());

    for shard in shards.into_iter() {
        let shard = match shard {
            Some(x) => x,
            None    => panic!("Missing shard"),
        };
        result.push(shard);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zero_len_shard() {
        assert_eq!(vec![].into_boxed_slice(), make_zero_len_shard());
    }

    #[test]
    fn test_zero_len_shards() {
        assert_eq!(vec![vec![].into_boxed_slice(), vec![].into_boxed_slice()],
                   make_zero_len_shards(2));
    }
}
