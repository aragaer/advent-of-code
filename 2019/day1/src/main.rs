use std::cmp;
use std::env;
use std::fs;
use std::io;

fn fuel_for(mass: i32) -> i32 {
    cmp::max(mass / 3 - 2, 0)
}

fn read_input() -> String {
    let mut contents = String::new();
    match env::args().nth(1) {
        None => Box::new(io::stdin()) as Box<dyn io::Read>,
        Some(filename) => Box::new(fs::File::open(filename)
                                   .expect("Failed to open file")),
    }.read_to_string(&mut contents)
        .expect("Failed to read");

    contents
}

fn main() {
    let mut result = 0;
    let mut result2 = 0;
    for line in read_input().lines() {
        let mass: i32 = line.parse()
            .expect("Failed to parse integer");

        let mut fuel = fuel_for(mass);

        result += fuel;
        while fuel > 0 {
            result2 += fuel;
            fuel = fuel_for(fuel);
        }
    }

    println!("Result: {}", result);
    println!("Result2: {}", result2);
}
