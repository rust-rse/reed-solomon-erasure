#![allow(dead_code)]
mod galois;
mod matrix;

use std::rc::Rc;
use std::cell::RefCell;
use std::ops::Deref;

use matrix::Matrix;

#[derive(Debug)]
pub enum Error {
    NotEnoughShards
}

pub type Shard = Rc<RefCell<Box<[u8]>>>;

pub fn boxed_u8_into_shard(b : Box<[u8]>) -> Shard {
    Rc::new(RefCell::new(b))
}

pub fn make_zero_len_shard() -> Shard {
    boxed_u8_into_shard(Box::new([]))
}

pub fn make_zero_len_shards(count : usize) -> Vec<Shard> {
    let mut result = Vec::with_capacity(count);
    for _ in 0..count {
        result.push(make_zero_len_shard());
    }
    result
}

pub fn make_blank_shard(size : usize) -> Shard {
    boxed_u8_into_shard(vec![0; size].into_boxed_slice())
}

pub fn make_blank_shards(size : usize, count : usize) -> Vec<Shard> {
    let mut result = Vec::with_capacity(count);
    for _ in 0..count {
        result.push(make_blank_shard(size));
    }
    result
}

pub struct ReedSolomon {
    data_shard_count   : usize,
    parity_shard_count : usize,
    total_shard_count  : usize,
    matrix             : Matrix,
    parity_rows        : Vec<Shard>,
}

impl Clone for ReedSolomon {
    fn clone(&self) -> ReedSolomon {
        let mut parity_rows =
            Vec::with_capacity(self.parity_rows.len());

        for shard in self.parity_rows.iter() {
            let inner : RefCell<Box<[u8]>> = shard.deref().clone();
            parity_rows.push(Rc::new(inner));
        }

        ReedSolomon {
            data_shard_count   : self.data_shard_count,
            parity_shard_count : self.parity_shard_count,
            total_shard_count  : self.total_shard_count,
            matrix             : Matrix::clone(&self.matrix),
            parity_rows
        }
    }
}

impl ReedSolomon {
    fn build_matrix(data_shards : usize, total_shards : usize) -> Matrix {
        let vandermonde = Matrix::vandermonde(total_shards, data_shards);

        let top = vandermonde.sub_matrix(0, 0, data_shards, data_shards);

        vandermonde.multiply(&top.invert().unwrap())
    }

    pub fn new(data_shards : usize, parity_shards : usize) -> ReedSolomon {
        if 256 < data_shards + parity_shards {
            panic!("Too many shards, max is 256")
        }

        let total_shards = data_shards + parity_shards;
        let matrix       = Self::build_matrix(data_shards, total_shards);
        let mut parity_rows  = Vec::with_capacity(parity_shards);
        for i in 0..parity_shards {
            parity_rows.push(
                Rc::new(
                    RefCell::new(matrix.get_row(data_shards + i))));
        }

        ReedSolomon {
            data_shard_count   : data_shards,
            parity_shard_count : parity_shards,
            total_shard_count  : total_shards,
            matrix,
            parity_rows
        }
    }

    fn calc_offset(offset : Option<usize>) -> usize {
        match offset {
            Some(x) => x,
            None    => 0
        }
    }

    fn calc_byte_count(shards     : &[Shard],
                       byte_count : Option<usize>) -> usize {
        match byte_count {
            Some(x) => x,
            None    => shards[0].borrow().len()
        }
    }

    fn calc_byte_count_option_shards(shards     : &[Option<Shard>],
                                     offset     : usize,
                                     byte_count : Option<usize>) -> usize {
        match byte_count {
            Some(x) => x,
            None    => {
                for v in shards.iter() {
                    match *v {
                        Some(ref x) => return x.borrow().len() - offset,
                        None    => {},
                    }
                };
                0
            }
        }
    }

    pub fn data_shard_count(&self) -> usize {
        self.data_shard_count
    }

    pub fn parity_shard_count(&self) -> usize {
        self.parity_shard_count
    }

    pub fn total_shard_count(&self) -> usize {
        self.total_shard_count
    }

    fn check_buffer_and_sizes(&self,
                              shards : &Vec<Shard>,
                              offset : usize, byte_count : usize) {
        if shards.len() != self.total_shard_count {
            panic!("Incorrect number of shards : {}", shards.len())
        }

        let shard_length = shards[0].borrow().len();
        for shard in shards.iter() {
            if shard.borrow().len() != shard_length {
                panic!("Shards are of different sizes");
            }
        }

        if shard_length < offset + byte_count {
            panic!("Buffers too small, shard_length : {}, offset + byte_count : {}", shard_length, offset + byte_count);
        }
    }

    fn check_buffer_and_sizes_option_shards(&self,
                                            shards : &Vec<Option<Shard>>,
                                            offset : usize, byte_count : usize) {
        if shards.len() != self.total_shard_count {
            panic!("Incorrect number of shards : {}", shards.len())
        }

        let mut shard_length = None;
        for shard in shards.iter() {
            if let Some(ref s) = *shard {
                match shard_length {
                    None    => shard_length = Some(s.borrow().len()),
                    Some(x) => {
                        if s.borrow().len() != x {
                            panic!("Shards are of different sizes");
                        }
                    }
                }
            }
        }

        if let Some(x) = shard_length {
            if x < offset + byte_count {
                panic!("Buffers too small, shard_length : Some({}), offset + byte_count : {}", x, offset + byte_count);
            }
        }
    }

    fn code_first_input_shard(matrix_rows  : &Vec<Shard>,
                              outputs      : &mut [Shard],
                              output_count : usize,
                              offset       : usize,
                              byte_count   : usize,
                              i_input      : usize,
                              input_shard  : &Box<[u8]>) {
        let table = &galois::MULT_TABLE;

        for i_output in 0..output_count {
            let mut output_shard =
                outputs[i_output].borrow_mut();
            let matrix_row       = matrix_rows[i_output].borrow();
            let mult_table_row   = table[matrix_row[i_input] as usize];
            for i_byte in offset..offset + byte_count {
                output_shard[i_byte] =
                    mult_table_row[input_shard[i_byte] as usize];
            }
        }
    }

    fn code_other_input_shard(matrix_rows  : &Vec<Shard>,
                              outputs      : &mut [Shard],
                              output_count : usize,
                              offset       : usize,
                              byte_count   : usize,
                              i_input      : usize,
                              input_shard  : &Box<[u8]>) {
        let table = &galois::MULT_TABLE;

        for i_output in 0..output_count {
            let mut output_shard = outputs[i_output].borrow_mut();
            let matrix_row       = matrix_rows[i_output].borrow();
            let mult_table_row   = &table[matrix_row[i_input] as usize];
            for i_byte in offset..offset + byte_count {
                output_shard[i_byte] ^= mult_table_row[input_shard[i_byte] as usize];
            }
        }
    }

    // Translated from InputOutputByteTableCodingLoop.java
    fn code_some_shards(matrix_rows  : &Vec<Shard>,
                        inputs       : &[Shard],
                        input_count  : usize,
                        outputs      : &mut [Shard],
                        output_count : usize,
                        offset       : usize,
                        byte_count   : usize) {
        {
            let i_input = 0;
            let input_shard = inputs[i_input].borrow();
            Self::code_first_input_shard(matrix_rows,
                                         outputs, output_count,
                                         offset,  byte_count,
                                         i_input, &input_shard);
        }

        for i_input in 1..input_count {
            let input_shard = inputs[i_input].borrow();
            Self::code_other_input_shard(matrix_rows,
                                         outputs, output_count,
                                         offset, byte_count,
                                         i_input, &input_shard);
        }
    }

    fn code_some_option_shards(matrix_rows  : &Vec<Shard>,
                               inputs       : &[Option<Shard>],
                               input_count  : usize,
                               outputs      : &mut [Shard],
                               output_count : usize,
                               offset       : usize,
                               byte_count   : usize) {
        {
            let i_input = 0;
            let input_shard = match inputs[i_input] {
                Some(ref x) => x.borrow(),
                None        => panic!()
            };
            Self::code_first_input_shard(matrix_rows,
                                         outputs, output_count,
                                         offset,  byte_count,
                                         i_input, &input_shard);
        }

        for i_input in 1..input_count {
            let input_shard = match inputs[i_input] {
                Some(ref x) => x.borrow(),
                None        => panic!()
            };
            Self::code_other_input_shard(matrix_rows,
                                         outputs, output_count,
                                         offset, byte_count,
                                         i_input, &input_shard);
        }
    }

    pub fn encode_parity(&self,
                         shards     : &mut Vec<Shard>,
                         offset     : Option<usize>,
                         byte_count : Option<usize>) {
        let offset     = Self::calc_offset(offset);
        let byte_count = Self::calc_byte_count(shards, byte_count);

        self.check_buffer_and_sizes(shards, offset, byte_count);

        let (inputs, outputs) = shards.split_at_mut(self.data_shard_count);

        Self::code_some_shards(&self.parity_rows,
                               inputs,  self.data_shard_count,
                               outputs, self.parity_shard_count,
                               offset, byte_count);
    }

    // Translated from CodingLoopBase.java
    fn check_some_shards(matrix_rows : &Vec<Shard>,
                         inputs      : &[Shard],
                         input_count : usize,
                         to_check    : &[Shard],
                         check_count : usize,
                         offset      : usize,
                         byte_count  : usize)
                         -> bool {
        let table = &galois::MULT_TABLE;

        for i_byte in offset..offset + byte_count {
            for i_output in 0..check_count {
                let matrix_row = matrix_rows[i_output as usize].borrow();
                let mut value = 0;
                for i_input in 0..input_count {
                    value ^=
                        table
                        [matrix_row[i_input]     as usize]
                        [inputs[i_input].borrow()[i_byte] as usize];
                }
                if to_check[i_output].borrow()[i_byte] != value {
                    return false
                }
            }
        }
        true
    }

    pub fn is_parity_correct(&self,
                             shards     : &Vec<Shard>,
                             offset     : Option<usize>,
                             byte_count : Option<usize>) -> bool {
        let offset     = Self::calc_offset(offset);
        let byte_count = Self::calc_byte_count(shards, byte_count);

        self.check_buffer_and_sizes(shards, offset, byte_count);

        let (data_shards, to_check) = shards.split_at(self.data_shard_count);

        Self::check_some_shards(&self.parity_rows,
                                data_shards, self.data_shard_count,
                                to_check,    self.parity_shard_count,
                                offset, byte_count)
    }

    pub fn shards_to_option_shards(shards : &Vec<Shard>)
                                   -> Vec<Option<Shard>> {
        let mut result = Vec::with_capacity(shards.len());

        for v in shards.iter() {
            let inner : RefCell<Box<[u8]>> = v.deref().clone();
            result.push(Some(Rc::new(inner)));
        }
        result
    }

    pub fn shards_into_option_shards(shards : Vec<Shard>)
                                     -> Vec<Option<Shard>> {
        let mut result = Vec::with_capacity(shards.len());

        for v in shards.into_iter() {
            result.push(Some(v));
        }
        result
    }

    pub fn option_shards_to_shards(shards : &Vec<Option<Shard>>,
                                   offset : Option<usize>,
                                   count  : Option<usize>)
                                   -> Vec<Shard> {
        let offset = Self::calc_offset(offset);
        let count  = match count {
            None    => shards.len(),
            Some(x) => x
        };

        let mut result = Vec::with_capacity(shards.len());

        for i in offset..offset + count {
            let shard = match shards[i] {
                Some(ref x) => x,
                None        => panic!("Missing shards, index : {}", i),
            };
            let inner : RefCell<Box<[u8]>> = shard.deref().clone();
            result.push(Rc::new(inner));
        }
        result
    }

    pub fn option_shards_into_shards(shards : Vec<Option<Shard>>)
                                     -> Vec<Shard> {
        let mut result = Vec::with_capacity(shards.len());

        for shard in shards.into_iter() {
            let shard = match shard {
                Some(x) => x,
                None    => panic!("Missing shards"),
            };
            result.push(shard);
        }
        result
    }

    pub fn decode_missing(&self,
                          shards     : &mut Vec<Option<Shard>>,
                          offset     : Option<usize>,
                          byte_count : Option<usize>)
                          -> Result<(), Error> {
        let offset     = Self::calc_offset(offset);
        let byte_count = Self::calc_byte_count_option_shards(shards,
                                                             offset,
                                                             byte_count);

        self.check_buffer_and_sizes_option_shards(shards, offset, byte_count);

        // Quick check: are all of the shards present?  If so, there's
        // nothing to do.
        let mut number_present = 0;
        for v in shards.iter() {
            if let Some(_) = *v { number_present += 1; }
        }
        if number_present == self.total_shard_count {
            // Cool.  All of the shards data data.  We don't
            // need to do anything.
            return Ok(())
        }

        // More complete sanity check
        if number_present < self.data_shard_count {
            return Err(Error::NotEnoughShards)
        }

        // Pull out the rows of the matrix that correspond to the
        // shards that we have and build a square matrix.  This
        // matrix could be used to generate the shards that we have
        // from the original data.
        //
        // Also, pull out an array holding just the shards that
        // correspond to the rows of the submatrix.  These shards
        // will be the input to the decoding process that re-creates
        // the missing data shards.
        let mut sub_matrix =
            Matrix::new(self.data_shard_count, self.data_shard_count);
        let mut sub_shards : Vec<Shard> =
            Vec::with_capacity(self.data_shard_count);
        {
            for matrix_row in 0..self.total_shard_count {
                let sub_matrix_row = sub_shards.len();

                if sub_matrix_row >= self.data_shard_count { break; }

                if let Some(ref shard) = shards[matrix_row] {
                    for c in 0..self.data_shard_count {
                        sub_matrix.set(sub_matrix_row, c,
                                       self.matrix.get(matrix_row, c));
                    }
                    sub_shards.push(Rc::clone(shard));
                }
            }
        }

        // Invert the matrix, so we can go from the encoded shards
        // back to the original data.  Then pull out the row that
        // generates the shard that we want to decode.  Note that
        // since this matrix maps back to the orginal data, it can
        // be used to create a data shard, but not a parity shard.
        let data_decode_matrix = sub_matrix.invert().unwrap();

        // Re-create any data shards that were missing.
        //
        // The input to the coding is all of the shards we actually
        // have, and the output is the missing data shards.  The computation
        // is done using the special decode matrix we just built.
        let mut matrix_rows : Vec<Shard> =
            make_zero_len_shards(self.parity_shard_count);
        {
            let mut outputs : Vec<Shard> =
                make_blank_shards(offset + byte_count,
                                  self.parity_shard_count);
            let mut output_count = 0;
            for i_shard in 0..self.data_shard_count {
                if let None = shards[i_shard] {
                    // link slot in outputs to the missing slot in shards
                    shards[i_shard] =
                        Some(Rc::clone(&outputs[output_count]));
                    matrix_rows[output_count] =
                        boxed_u8_into_shard(
                            data_decode_matrix.get_row(i_shard));
                    output_count += 1;
                }
            }
            Self::code_some_shards(&matrix_rows,
                                   &sub_shards,  self.data_shard_count,
                                   &mut outputs, output_count,
                                   offset, byte_count);
        }

        // Now that we have all of the data shards intact, we can
        // compute any of the parity that is missing.
        //
        // The input to the coding is ALL of the data shards, including
        // any that we just calculated.  The output is whichever of the
        // data shards were missing.
        {
            let mut outputs : Vec<Shard> =
                make_blank_shards(offset + byte_count,
                                  self.parity_shard_count);
            let mut output_count = 0;
            for i_shard in self.data_shard_count..self.total_shard_count {
                if let None = shards[i_shard] {
                    // link slot in outputs to the missing slot in shards
                    shards[i_shard] =
                        Some(Rc::clone(&outputs[output_count]));
                    matrix_rows[output_count] =
                        Rc::clone(
                            &self.parity_rows[i_shard
                                              - self.data_shard_count]);
                    output_count += 1;
                }
            }
            Self::code_some_option_shards(&matrix_rows,
                                          &shards, self.data_shard_count,
                                          &mut outputs, output_count,
                                          offset, byte_count);
        }

        Ok (())
    }
}

#[cfg(test)]
mod tests {
    extern crate rand;

    use super::*;

    macro_rules! make_random_shards {
        ($size:expr, $per_shard:expr) => {{
            let mut shards = Vec::with_capacity(13);
            for _ in 0..$size {
                shards.push(make_blank_shard($per_shard));
            }

            for s in shards.iter_mut() {
                fill_random(s);
            }

            shards
        }}
    }

    macro_rules! shards {
        (
            $( [ $( $x:expr ),* ] ),*
        ) => {{
            let shards : Vec<Shard> =
                vec![ $( boxed_u8_into_shard(Box::new([ $( $x ),* ])) ),* ];
            shards
        }}
    }

    fn is_increasing_and_contains_data_row(indices : &Vec<usize>) -> bool {
        let cols = indices.len();
        for i in 0..cols-1 {
            if indices[i] >= indices[i+1] {
                return false
            }
        }
        return indices[0] < cols
    }

    fn increment_indices(indices : &mut Vec<usize>,
                         index_bound : usize) -> bool {
        for i in (0..indices.len()).rev() {
            indices[i] += 1;
            if indices[i] < index_bound {
                break;
            }

            if i == 0 {
                return false
            }

            indices[i] = 0
        }

        return true
    }

    fn increment_indices_until_increasing_and_contains_data_row(indices : &mut Vec<usize>, max_index : usize) -> bool {
        loop {
            let valid = increment_indices(indices, max_index);
            if !valid {
                return false
            }

            if is_increasing_and_contains_data_row(indices) {
                return true
            }
        }
    }

    fn find_singular_sub_matrix(m : Matrix) -> Option<Matrix> {
        let rows = m.row_count();
        let cols = m.column_count();
        let mut row_indices = Vec::with_capacity(cols);
        while increment_indices_until_increasing_and_contains_data_row(&mut row_indices, rows) {
            let mut sub_matrix = Matrix::new(cols, cols);
            for i in 0..row_indices.len() {
                let r = row_indices[i];
                for c in 0..cols {
                    sub_matrix.set(i, c, m.get(r, c));
                }
            }

            match sub_matrix.invert() {
                Err(matrix::Error::SingularMatrix) => return Some(sub_matrix),
                whatever => whatever.unwrap()
            };
        }
        None
    }

    fn fill_random(arr : &mut Shard) {
        for a in arr.borrow_mut().iter_mut() {
            *a = rand::random::<u8>();
        }
    }

    #[test]
    fn test_encoding() {
        let per_shard = 50_000;

        let r = ReedSolomon::new(10, 3);

        let mut shards = make_random_shards!(13, per_shard);

        r.encode_parity(&mut shards, None, None);
        assert!(r.is_parity_correct(&shards, None, None));
    }

    #[test]
    fn test_decode_missing() {
        let per_shard = 100_000;

        let r = ReedSolomon::new(8, 5);

        let mut shards = make_random_shards!(13, per_shard);

        r.encode_parity(&mut shards, None, None);

        let master_copy = shards.clone();

        let mut shards = ReedSolomon::shards_to_option_shards(&shards);

        // Try to decode with all shards present
        r.decode_missing(&mut shards,
                         None, None).unwrap();
        {
            let shards = ReedSolomon::option_shards_to_shards(&shards, None, None);
            assert!(r.is_parity_correct(&shards, None, None));
            assert_eq!(shards, master_copy);
        }

        // Try to decode with 10 shards
        shards[0] = None;
        shards[2] = None;
        //shards[4] = None;
        r.decode_missing(&mut shards,
                         None, None).unwrap();
        {
            let shards = ReedSolomon::option_shards_to_shards(&shards, None, None);
            assert!(r.is_parity_correct(&shards, None, None));
            assert_eq!(shards, master_copy);
        }

	      // Try to deocde with 6 data and 4 parity shards
        shards[0] = None;
        shards[2] = None;
        shards[12] = None;
        r.decode_missing(&mut shards,
                         None, None).unwrap();
        {
            let shards = ReedSolomon::option_shards_to_shards(&shards, None, None);
            assert!(r.is_parity_correct(&shards, None, None));
            assert_eq!(shards, master_copy);
        }

        // Try to decode with 7 data and 1 parity shards
        shards[0] = None;
        shards[1] = None;
        shards[9] = None;
        shards[10] = None;
        shards[11] = None;
        shards[12] = None;
        match r.decode_missing(&mut shards,
                               None, None) {
            Err(Error::NotEnoughShards) => {},
            Ok(()) => panic!("Should fail due to not enough shards"),
        }
    }

    #[test]
    fn test_is_parity_correct() {
        let per_shard = 33_333;

        let r = ReedSolomon::new(10, 4);

        let mut shards = make_random_shards!(14, per_shard);

        r.encode_parity(&mut shards, None, None);
        assert!(r.is_parity_correct(&shards, None, None));

        // corrupt shards
        fill_random(&mut shards[5]);
        assert!(!r.is_parity_correct(&shards, None, None));

        // Re-encode
        r.encode_parity(&mut shards, None, None);
        fill_random(&mut shards[1]);
        assert!(!r.is_parity_correct(&shards, None, None));
    }

    #[test]
    fn test_one_encode() {
        let r = ReedSolomon::new(5, 5);

        let mut shards = shards!([0, 1],
                                 [4, 5],
                                 [2, 3],
                                 [6, 7],
                                 [8, 9],
                                 [0, 0],
                                 [0, 0],
                                 [0, 0],
                                 [0, 0],
                                 [0, 0]);

        r.encode_parity(&mut shards, None, None);
        { assert_eq!(shards[5].borrow()[0], 12);
          assert_eq!(shards[5].borrow()[1], 13); }
        { assert_eq!(shards[6].borrow()[0], 10);
          assert_eq!(shards[6].borrow()[1], 11); }
        { assert_eq!(shards[7].borrow()[0], 14);
          assert_eq!(shards[7].borrow()[1], 15); }
        { assert_eq!(shards[8].borrow()[0], 90);
          assert_eq!(shards[8].borrow()[1], 91); }
        { assert_eq!(shards[9].borrow()[0], 94);
          assert_eq!(shards[9].borrow()[1], 95); }

        assert!(r.is_parity_correct(&shards, None, None));

        shards[8].borrow_mut()[0] += 1;
        assert!(!r.is_parity_correct(&shards, None, None));
    }
}
