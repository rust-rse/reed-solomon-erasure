use super::Shard;

mod helper {
    use super::*;

    pub fn calc_offset(offset : Option<usize>) -> usize {
        match offset {
            Some(x) => x,
            None    => 0
        }
    }

    /*pub fn calc_byte_count(shards     : &Vec<Shard>,
                           byte_count : Option<usize>) -> usize {
        let result = match byte_count {
            Some(x) => x,
            None    => shards[0].len()
        };

        if result == 0 { panic!("Byte count is zero"); }

        result
    }*/

    /*pub fn calc_offset_and_byte_count(offset : Option<usize>,
                                      shards : &Vec<Shard>,
                                      byte_count : Option<usize>)
                                      -> (usize, usize) {
        let offset     = calc_offset(offset);
        let byte_count = calc_byte_count(shards, byte_count);

        (offset, byte_count)
    }*/

    /*pub fn calc_byte_count_option_shards(shards     : &Vec<Option<Shard>>,
                                         byte_count : Option<usize>) -> usize {
        let result = match byte_count {
            Some(x) => x,
            None    => {
                let mut value = None;
                for v in shards.iter() {
                    match *v {
                        Some(ref x) => { value = Some(x.len());
                                         break; },
                        None        => {},
                    }
                };
                match value {
                    Some(v) => v,
                    None    => panic!("No shards are present")
                }
            }
        };

        if result == 0 { panic!("Byte count is zero"); }

        result
    }*/

    /*pub fn calc_offset_and_byte_count_option_shards(offset : Option<usize>,
                                                    shards : &Vec<Option<Shard>>,
                                                    byte_count : Option<usize>)
                                                    -> (usize, usize) {
        let offset     = calc_offset(offset);
        let byte_count = calc_byte_count_option_shards(shards, byte_count);

        (offset, byte_count)
    }*/
}


/// Makes shard with byte array of zero length
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

/// Makes shard with byte array filled with zeros of some length
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

/// Transforms vector of shards to vector of option shards
///
/// # Remarks
///
/// Each shard is cloned rather than moved, which may be slow.
///
/// This is mainly useful when you want to repair a vector
/// of shards using `decode_missing`.
pub fn shards_to_option_shards(shards : &Vec<Shard>)
                               -> Vec<Option<Shard>> {
    let mut result = Vec::with_capacity(shards.len());

    for v in shards.iter() {
        let inner : Box<[u8]> = v.clone();
        result.push(Some(inner));
    }
    result
}

/// Transforms vector of shards into vector of option shards
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

/// Transforms a section of vector of option shards to vector of shards
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
/// Panics when any of the shards is missing or the range exceeds number of shards provided.
pub fn option_shards_to_shards(shards : &Vec<Option<Shard>>,
                               start  : Option<usize>,
                               count  : Option<usize>)
                               -> Vec<Shard> {
    let offset = helper::calc_offset(start);
    let count  = match count {
        None    => shards.len(),
        Some(x) => x
    };

    if shards.len() < offset + count {
        panic!("Too few shards, number of shards : {}, offset + count : {}", shards.len(), offset + count);
    }

    let mut result = Vec::with_capacity(shards.len());

    for i in offset..offset + count {
        let shard = match shards[i] {
            Some(ref x) => x,
            None        => panic!("Missing shard, index : {}", i),
        };
        let inner : Box<[u8]> = shard.clone();
        result.push(inner);
    }
    result
}

/// Transforms vector of option shards into vector of shards
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

/// Deep copies vector of shards
///
/// # Remarks
///
/// Normally doing `shards.clone()` (where `shards` is a `Vec<Shard>`) is okay,
/// but the `Rc` in `Shard`'s definition will cause it to be a shallow copy, rather
/// than a deep copy.
///
/// If the shards are used immutably, then a shallow copy is more desirable, as it
/// has significantly lower overhead.
///
/// If the shards are used mutably, then a deep copy may be more desirable, as this
/// will avoid unexpected bugs caused by multiple ownership.
pub fn deep_clone_shards(shards : &Vec<Shard>) -> Vec<Shard> {
    let mut result = Vec::with_capacity(shards.len());

    for v in shards.iter() {
        let inner : Box<[u8]> = v.clone();
        result.push(inner);
    }
    result
}

/// Deep copies vector of option shards
///
/// # Remarks
///
/// Normally doing `shards.clone()` (where `shards` is a `Vec<Option<Shard>>`) is okay,
/// but the `Rc` in `Shard`'s definition will cause it to be a shallow copy, rather
/// than a deep copy.
///
/// If the shards are used immutably, then a shallow copy is more desirable, as it
/// has significantly lower overhead.
///
/// If the shards are used mutably, then a deep copy may be more desirable, as this
/// will avoid unexpected bugs caused by multiple ownership.
pub fn deep_clone_option_shards(shards : &Vec<Option<Shard>>) -> Vec<Option<Shard>> {
    let mut result = Vec::with_capacity(shards.len());

    for v in shards.iter() {
        let inner = match *v {
            Some(ref x) => { let inner = x.clone();
                             Some(inner) },
            None        => None
        };
        result.push(inner);
    }
    result
}
