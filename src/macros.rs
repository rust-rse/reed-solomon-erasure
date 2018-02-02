/// Makes it easier to work with 2D slices, arrays, etc.
///
/// # Examples
/// ## Byte arrays on stack to `Vec<&[u8]>`
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # fn main () {
/// let array : [[u8; 3]; 2] = [[1, 2, 3],
///                             [4, 5, 6]];
///
/// let refs : Vec<&[u8]> =
///     convert_2D_slices!(array =>to_vec &[u8]);
/// # }
/// ```
/// ## Byte arrays on stack to `Vec<&mut [u8]>` (borrow mutably)
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # fn main () {
/// let mut array : [[u8; 3]; 2] = [[1, 2, 3],
///                                 [4, 5, 6]];
///
/// let refs : Vec<&mut [u8]> =
///     convert_2D_slices!(array =>to_mut_vec &mut [u8]);
/// # }
/// ```
/// ## Byte arrays on stack to `SmallVec<[&mut [u8]; 32]>` (borrow mutably)
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # extern crate smallvec;
/// # use smallvec::SmallVec;
/// # fn main () {
/// let mut array : [[u8; 3]; 2] = [[1, 2, 3],
///                                 [4, 5, 6]];
///
/// let refs : SmallVec<[&mut [u8]; 32]> =
///     convert_2D_slices!(array =>to_mut SmallVec<[&mut [u8]; 32]>,
///                        SmallVec::with_capacity);
/// # }
/// ```
/// ## Shard array to `SmallVec<[&mut [u8]; 32]>` (borrow mutably)
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # extern crate smallvec;
/// # use smallvec::SmallVec;
/// # fn main () {
/// let mut shards = shards!([1, 2, 3],
///                          [4, 5, 6]);
///
/// let refs : SmallVec<[&mut [u8]; 32]> =
///     convert_2D_slices!(shards =>to_mut SmallVec<[&mut [u8]; 32]>,
///                        SmallVec::with_capacity);
/// # }
/// ```
/// ## Shard array to `Vec<&mut [u8]>` (borrow mutably) into `SmallVec<[&mut [u8]; 32]>` (move)
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # extern crate smallvec;
/// # use smallvec::SmallVec;
/// # fn main () {
/// let mut shards = shards!([1, 2, 3],
///                          [4, 5, 6]);
///
/// let refs1 = convert_2D_slices!(shards =>to_mut_vec &mut [u8]);
///
/// let refs2 : SmallVec<[&mut [u8]; 32]> =
///     convert_2D_slices!(refs1 =>into SmallVec<[&mut [u8]; 32]>,
///                        SmallVec::with_capacity);
/// # }
/// ```
#[macro_export]
macro_rules! convert_2D_slices {
    (
        $slice:expr =>into_vec $dst_type:ty
    ) => {
        convert_2D_slices!($slice =>into Vec<$dst_type>,
                           Vec::with_capacity)
    };
    (
        $slice:expr =>to_vec $dst_type:ty
    ) => {
        convert_2D_slices!($slice =>to Vec<$dst_type>,
                           Vec::with_capacity)
    };
    (
        $slice:expr =>to_mut_vec $dst_type:ty
    ) => {
        convert_2D_slices!($slice =>to_mut Vec<$dst_type>,
                           Vec::with_capacity)
    };
    (
        $slice:expr =>into $dst_type:ty, $with_capacity:path
    ) => {{
        let mut result : $dst_type =
            $with_capacity($slice.len());
        for i in $slice.into_iter() {
            result.push(i);
        }
        result
    }};
    (
        $slice:expr =>to $dst_type:ty, $with_capacity:path
    ) => {{
        let mut result : $dst_type =
            $with_capacity($slice.len());
        for i in $slice.iter() {
            result.push(i);
        }
        result
    }};
    (
        $slice:expr =>to_mut $dst_type:ty, $with_capacity:path
    ) => {{
        let mut result : $dst_type =
            $with_capacity($slice.len());
        for i in $slice.iter_mut() {
            result.push(i);
        }
        result
    }}
}
