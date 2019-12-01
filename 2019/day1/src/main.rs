use std::cmp;
use std::env;
use std::fs;

fn fuel_for(mass: i32) -> i32 {
    cmp::max(mass / 3 - 2, 0)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let contents = fs::read_to_string(&args[1])
        .expect("Failed to read file");

    let mut result = 0;
    let mut result2 = 0;
    for line in contents.lines() {
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
