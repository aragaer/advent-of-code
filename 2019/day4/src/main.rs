fn main() {
    let mut result = 0;
    let mut result2 = 0;
    'outer: for mut x in 136818..(685979+1) {
        let mut last = 10;
        let mut group_size = 1;
        let mut have_group = false;
        let mut have_group_2 = false;
        while x > 0 {
            let digit = x % 10;
            if digit > last {
                continue 'outer;
            }
            if digit == last {
                group_size += 1;
            } else {
                last = digit;
                have_group |= group_size > 1;
                have_group_2 |= group_size == 2;
                group_size = 1;
            }
            x /= 10;
        }
        have_group |= group_size > 1;
        have_group_2 |= group_size == 2;

        if have_group {
            result += 1;
        }
        if have_group_2 {
            result2 += 1;
        }
    }
    println!("Result: {}", result);
    println!("Result2: {}", result2);
}
