#![allow(dead_code)]

use std::cell::Cell;

pub fn split_slice_mut_with_index<'a, T> (slice      : &'a mut [T],
                                          chunk_size : usize)
                                          -> Vec<(usize, &'a mut [T])> {
    let mut rem_len   = slice.len();
    let mut cur_index = 0;
    let mut result    = Vec::with_capacity(slice.len() / chunk_size + 1);
    let rem_slice     = Cell::new(slice);

    loop {
        if chunk_size < rem_len {
            let slice = rem_slice.take();
            let (l, r) = slice.split_at_mut(chunk_size);
            result.push((cur_index, l));
            rem_slice.set(r);
        } else if rem_len > 0 {
            result.push((cur_index, rem_slice.take()));
            break;
        } else {
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
    let rem_slice     = Cell::new(slice);

    loop {
        if chunk_size < rem_len {
            let slice = rem_slice.take();
            let (l, r) = slice.split_at(chunk_size);
            result.push((cur_index, l));
            rem_slice.set(r);
        } else if rem_len > 0 {
            result.push((cur_index, rem_slice.take()));
            break;
        } else {
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

pub fn breakdown_slice_mut_with_index<'a, T>(slice : &'a mut [T]) -> Vec<(usize, &'a mut T)> {
    let mut result = Vec::with_capacity(slice.len());
    for (i, v) in split_slice_mut_with_index(slice, 1).into_iter() {
        result.push((i, &mut v[0]));
    }
    result
}

pub fn breakdown_slice_with_index<'a, T>(slice : &'a [T]) -> Vec<(usize, &'a T)> {
    let mut result = Vec::with_capacity(slice.len());
    for (i, v) in split_slice_with_index(slice, 1).into_iter() {
        result.push((i, & v[0]));
    }
    result
}

pub fn breakdown_slice_mut<'a, T>(slice : &'a mut [T]) -> Vec<&'a mut T> {
    let mut result = Vec::with_capacity(slice.len());
    for (_, v) in split_slice_mut_with_index(slice, 1).into_iter() {
        result.push(&mut v[0]);
    }
    result
}

pub fn breakdown_slice<'a, T>(slice : &'a [T]) -> Vec<&'a T> {
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

pub fn slices_are_equal<T>(slice1 : &[T],
                           slice2 : &[T]) -> bool
    where T : PartialEq
{
    if slice1.len() != slice2.len() {
        return false;
    }
    for i in 0..slice1.len() {
        if slice1[i] != slice2[i] {
            return false;
        }
    }
    true
}
