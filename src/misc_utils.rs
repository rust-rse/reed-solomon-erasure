use std::cell::Cell;
use std::cell::RefCell;

pub fn split_slice_mut_with_index<'a, T> (slice      : &'a mut [T],
                                          chunk_size : usize)
                                          -> Vec<(usize, &'a mut [T])> {
    let mut rem_len   = slice.len();
    let mut cur_index = 0;
    let mut result    = Vec::with_capacity(slice.len() / chunk_size + 1);
    let mut rem_slice = Vec::with_capacity(1);

    rem_slice.push(slice);

    loop {
        if chunk_size < rem_len {
            let slice = rem_slice.pop().unwrap();
            let (l, r) = slice.split_at_mut(1);
            result.push((cur_index, l));
            rem_slice.push(r);
        } else {
            result.push((cur_index, rem_slice.pop().unwrap()));
            break;
        }

        cur_index += 1;
        rem_len   -= chunk_size;
    }
    result
}

pub fn split_slice_with_index<'a, T> (slice      : &'a [T],
                                      chunk_size : usize)
                                      -> Vec<(usize, &'a [T])> {
    let mut rem_len   = slice.len();
    let mut cur_index = 0;
    let mut result    = Vec::with_capacity(slice.len() / chunk_size + 1);
    let mut rem_slice = Vec::with_capacity(1);

    rem_slice.push(slice);

    loop {
        if chunk_size < rem_len {
            let slice = rem_slice.pop().unwrap();
            let (l, r) = slice.split_at(1);
            result.push((cur_index, l));
            rem_slice.push(r);
        } else {
            result.push((cur_index, rem_slice.pop().unwrap()));
            break;
        }

        cur_index += 1;
        rem_len   -= chunk_size;
    }
    result
}

pub fn split_slice_mut<'a, T> (slice      : &'a mut [T],
                               chunk_size : usize)
                               -> Vec<&'a mut [T]> {
    let mut result = Vec::with_capacity(slice.len());
    for (_, v) in split_slice_mut_with_index(slice, chunk_size) {
        result.push(v);
    }
    result
}

pub fn split_slice<'a, T> (slice      : &'a [T],
                           chunk_size : usize)
                           -> Vec<&'a [T]> {
    let mut result = Vec::with_capacity(slice.len());
    for (_, v) in split_slice_with_index(slice, chunk_size) {
        result.push(v);
    }
    result
}

pub fn break_down_slice_mut_with_index<'a, T>(slice : &'a mut [T]) -> Vec<(usize, &'a mut T)> {
    let mut result = Vec::with_capacity(slice.len());
    for (i, v) in split_slice_mut_with_index(slice, 1).into_iter() {
        result.push((i, &mut v[0]));
    }
    result
}

pub fn break_down_slice_with_index<'a, T>(slice : &'a [T]) -> Vec<(usize, &'a T)> {
    let mut result = Vec::with_capacity(slice.len());
    for (i, v) in split_slice_with_index(slice, 1).into_iter() {
        result.push((i, & v[0]));
    }
    result
}

pub fn break_down_slice_mut<'a, T>(slice : &'a mut [T]) -> Vec<&'a mut T> {
    let mut result = Vec::with_capacity(slice.len());
    for (_, v) in split_slice_mut_with_index(slice, 1).into_iter() {
        result.push(&mut v[0]);
    }
    result
}

pub fn break_down_slice<'a, T>(slice : &'a [T]) -> Vec<&'a T> {
    let mut result = Vec::with_capacity(slice.len());
    for (_, v) in split_slice_with_index(slice, 1).into_iter() {
        result.push(& v[0]);
    }
    result
}

pub fn slice_to_vec_of_refs<'a, T>(slice : &'a [T]) -> Vec<&'a T> {
    let mut result = Vec::with_capacity(slice.len());
    for v in slice.iter() {
        result.push(v);
    }
    result
}

pub fn mut_slice_to_vec_of_mut_refs<'a, T>(slice : &'a mut [T])
                                           -> Vec<&'a mut T> {
    let mut result = Vec::with_capacity(slice.len());
    for v in slice.iter_mut() {
        result.push(v);
    }
    result
}
