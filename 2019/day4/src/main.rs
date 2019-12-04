extern crate itertools;

use itertools::Itertools;


fn main() {
    let mut result = 0;
    let mut result2 = 0;
    for x in 136818..(685979+1) {
        if x.to_string().chars()
            .zip(x.to_string().chars().skip(1))
            .any(|(f, s)| f > s) {
                continue;
            }

        let mut have_same = false;
        let mut have_same_non_grouped = false;
        for count in x.to_string().chars()
            .group_by(|x| *x)
            .into_iter()
            .map(|(_, g)| g.count()) {
                have_same |= count > 1;
                have_same_non_grouped |= count == 2;
            }
        if have_same {
            result += 1;
        }
        if have_same_non_grouped {
            result2 += 1;
        }
    }
    println!("Result: {}", result);
    println!("Result2: {}", result2);
}
