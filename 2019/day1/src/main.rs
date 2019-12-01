use std::cmp;
use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::os::unix::io::FromRawFd;

fn fuel_for(mass: i32) -> i32 {
    cmp::max(mass / 3 - 2, 0)
}

fn input() -> impl Iterator<Item=io::Result<String>> {
    let reader = io::BufReader::new(match env::args().nth(1) {
        None => unsafe {fs::File::from_raw_fd(0)},
        Some(filename) => fs::File::open(filename).unwrap(),
    });
    reader.split('\n' as u8)
        .map(|b| b.unwrap())
        .map(String::from_utf8)
        .map(|s| Ok(s.unwrap()))
}

fn main() -> io::Result<()> {
    let mut result = 0;
    let mut result2 = 0;
    for line in input() {
        let mass: i32 = line?.parse().unwrap();
        let mut fuel = fuel_for(mass);
        result += fuel;
        while fuel > 0 {
            result2 += fuel;
            fuel = fuel_for(fuel);
        }
    }

    println!("Result: {}", result);
    println!("Result2: {}", result2);
    Ok(())
}
