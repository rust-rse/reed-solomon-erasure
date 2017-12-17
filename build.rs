use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

const FIELD_SIZE : usize = 256;

const GENERATING_POLYNOMIAL : usize = 29;

fn gen_log_table(polynomial : usize) -> [u8; FIELD_SIZE] {
    let mut result : [u8; FIELD_SIZE] = [0; FIELD_SIZE];
    let mut b      : usize            = 1;

    for log in 0..FIELD_SIZE-1 {
        result[b] = log as u8;

        b = b << 1;

        if FIELD_SIZE <= b {
            b = (b - FIELD_SIZE) ^ polynomial;
        }
    }

    result
}

const EXP_TABLE_SIZE : usize = FIELD_SIZE * 2 - 2;

fn gen_exp_table(log_table : &[u8; FIELD_SIZE]) -> [u8; EXP_TABLE_SIZE] {
    let mut result : [u8; EXP_TABLE_SIZE] = [0; EXP_TABLE_SIZE];

    for i in 1..FIELD_SIZE {
        let log                      = log_table[i] as usize;
        result[log]                  = i as u8;
        result[log + FIELD_SIZE - 1] = i as u8;
    }

    result
}

fn multiply(log_table : &[u8; FIELD_SIZE],
            exp_table : &[u8; EXP_TABLE_SIZE],
            a : u8,
            b : u8) -> u8 {
    if a == 0 || b == 0 {
        0
    }
    else {
        let log_a = log_table[a as usize];
        let log_b = log_table[b as usize];
        let log_result = log_a as usize + log_b as usize;
        exp_table[log_result]
    }
}

fn gen_mult_table(log_table : &[u8; FIELD_SIZE],
                  exp_table : &[u8; EXP_TABLE_SIZE])
                  -> [[u8; FIELD_SIZE]; FIELD_SIZE] {
    let mut result : [[u8; FIELD_SIZE]; FIELD_SIZE] = [[0; 256]; 256];

    for a in 0..FIELD_SIZE {
        for b in 0..FIELD_SIZE {
            result[a][b] = multiply(log_table, exp_table, a as u8, b as u8);
        }
    }

    result
}

macro_rules! write_table {
    (1D => $file:ident, $table:ident, $name:expr, $type:expr) => {{
        let len = $table.len();
        let mut table_str =
            String::from(format!("pub static {} : [{}; {}] = [", $name, $type, len));

        for v in $table.iter() {
            let str = format!("{}, ", v);
            table_str.push_str(&str);
        }

        table_str.push_str("];\n");

        $file.write_all(table_str.as_bytes()).unwrap();
    }};
    (2D => $file:ident, $table:ident, $name:expr, $type:expr) => {{
        let len1 = $table.len();
        let len2 = $table[0].len();
        let mut table_str =
            String::from(format!("pub static {} : [[{}; {}]; {}] = [",
                                 $name,
                                 $type,
                                 len1,
                                 len2));

        for a in $table.iter() {
            table_str.push_str("[");
            for b in a.iter() {
                let str = format!("{}, ", b);
                table_str.push_str(&str);
            }
            table_str.push_str("],\n");
        }

        table_str.push_str("];\n");

        $file.write_all(table_str.as_bytes()).unwrap();
    }}
}

fn main() {
    let log_table = gen_log_table(GENERATING_POLYNOMIAL);
    let exp_table = gen_exp_table(&log_table);
    let mult_table = gen_mult_table(&log_table, &exp_table);

    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("table.rs");
    let mut f = File::create(&dest_path).unwrap();

    write_table!(1D => f, log_table, "LOG_TABLE", "u8");
    write_table!(1D => f, exp_table, "EXP_TABLE", "u8");
    write_table!(2D => f, mult_table, "MULT_TABLE", "u8");
}
