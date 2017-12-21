pub fn split_slice_mut_with_index<'a, T> (slice      : &'a mut [T],
                                          chunk_size : usize)
                                          -> Vec<(usize, &'a mut [T])> {
    fn helper<'a, T>(slice      : &'a mut [T],
                     chunk_size : usize,
                     cur_index  : usize,
                     mut result : Vec<(usize, &'a mut [T])>)
                     -> Vec<(usize, &'a mut [T])> {
        if chunk_size < slice.len() {
            let (l, r) = slice.split_at_mut(chunk_size);
            result.push((cur_index, l));
            helper(r, chunk_size, cur_index + 1, result)
        }
        else {
            result.push((cur_index, slice));
            result
        }
    }

    let result = Vec::with_capacity(slice.len() / chunk_size + 1);
    helper(slice,
           chunk_size,
           0,
           result)
}

pub fn split_slice_with_index<'a, T> (slice      : &'a [T],
                                      chunk_size : usize)
                                      -> Vec<(usize, &'a [T])> {
    fn helper<'a, T>(slice      : &'a [T],
                     chunk_size : usize,
                     cur_index  : usize,
                     mut result : Vec<(usize, &'a [T])>)
                     -> Vec<(usize, &'a [T])> {
        if chunk_size < slice.len() {
            let (l, r) = slice.split_at(chunk_size);
            result.push((cur_index, l));
            helper(r, chunk_size, cur_index + 1, result)
        }
        else {
            result.push((cur_index, slice));
            result
        }
    }

    let result = Vec::with_capacity(slice.len() / chunk_size + 1);
    helper(slice,
           chunk_size,
           0,
           result)
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
