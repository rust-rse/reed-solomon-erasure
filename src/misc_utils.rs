use rayon::prelude::*;

#[cfg(test)]
pub fn fill_random(arr : &mut [u8]) {
    for a in arr.iter_mut() {
        *a = rand::random::<u8>();
    }
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

pub fn par_slices_are_equal<T>(slice1     : &[T],
                               slice2     : &[T],
                               chunk_size : usize) -> bool
    where T : PartialEq + Sync
{
    if slice1.len() != slice2.len() {
        return false;
    }

    // AUDIT
    //
    // Short-circuiting parallel equality check
    //
    // Terminologies and Symbols :
    //   f -> v : function f with some arguments returns v
    //   iff.   : if and only if
    //   <->    : double implication (iff.)
    //   ~      : negation (logic)
    //   forall : universal quantifier (first order logic)
    //   exists : existential quantifier (first order logic)
    //
    // The final `any` simply applies not to the input
    // to find the first false, then returns true if
    // the first false is found, and returns false
    // if no false is found
    //
    // This means
    //   `any` -> true AND possibly terminates early iff. a false is found
    //   `any` -> false                              iff. NO false is found
    //
    // Since a false means a part of slice1 does not match the corresponding
    // part of slice2(mismatch found), the above logic translates to
    //
    //   `any` -> true AND possibly terminates early iff. a mismatch is found
    //   `any` -> false                              iff. NO mismatch is found
    //
    // Overall `any` -> true means at least one mismatch is present
    //
    // Thus the defintion of the variable `at_least_one_mismatch_present` is
    // correct
    //
    // The negation of ">= 1 mismatch is present" is "no mismatch is present"
    //
    // Informal proof :
    //   ">= 1 mismatch is present" is the same as "there exists a mismatch"
    //
    //   Suppose P(x) is the predicate for equality, where x is a tuple for
    //   the corresponding parts of the two slices, then ~ P(x) indicates
    //   a mismatch
    //
    //   Then we can translate the logic to the following
    //
    //   ~ (exists x, ~ P(x)) <-> forall x, ~ (~ P(x))
    //                      <-> forall x, P(x)
    //
    //   which can be shown to be valid easily
    //
    //   Qed
    //
    // Thus this function returning `!at_least_one_mismatch_present` is correct
    // as that is only true when there is no mismatch, and only false when
    // mismatch is present

    let at_least_one_mismatch_present =
        slice1.par_chunks(chunk_size)
        .into_par_iter()
        .enumerate()
        .map(|(i, slice1_part)| {
            let slice2_start = i * chunk_size;
            let slice2_part  = &slice2[slice2_start..
                                       slice2_start + slice1_part.len()];
            slices_are_equal(slice1_part, slice2_part)
        })
        .any(|x| !x);

    !at_least_one_mismatch_present
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fill_random(arr : &mut [u8]) {
        for a in arr.iter_mut() {
            *a = rand::random::<u8>();
        }
    }

    #[test]
    fn slices_are_equal_same_as_par_slices_are_equal() {
        let len = 1000;

        let mut slice1 = vec![0; len];
        let mut slice2 = vec![0; len];

        for _ in 0..1000 {
            let chunk_size = rand::random::<usize>();

            fill_random(&mut slice1);
            slice2.copy_from_slice(&slice1);

            assert!(slices_are_equal(&slice1, &slice2));
            assert!(par_slices_are_equal(&slice1, &slice2, chunk_size));

            slice1[0] = 0;
            slice2[0] = 1;

            assert!(!slices_are_equal(&slice1, &slice2));
            assert!(!par_slices_are_equal(&slice1, &slice2, chunk_size));
        }
    }

    #[test]
    fn unequal_length_slices() {
        let slice1 = vec![0; 1000];
        let slice2 = vec![0;  999];

        assert!(!slices_are_equal(&slice1, &slice2));
        assert!(!par_slices_are_equal(&slice1, &slice2, 11));
    }
}
