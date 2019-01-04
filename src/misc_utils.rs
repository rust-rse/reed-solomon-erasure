#[cfg(test)]
pub fn fill_random(arr: &mut [u8]) {
    for a in arr.iter_mut() {
        *a = rand::random::<u8>();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fill_random(arr: &mut [u8]) {
        for a in arr.iter_mut() {
            *a = rand::random::<u8>();
        }
    }
}
