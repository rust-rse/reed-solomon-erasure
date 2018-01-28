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
