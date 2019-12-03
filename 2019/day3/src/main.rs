extern crate num;

use num::complex::Complex;
use std::cmp;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::os::unix::io::FromRawFd;

struct Segment {
    direction: Complex<i32>,
    length: u32,
}

fn read_segment(s: &str) -> Segment {
    Segment {
        direction: match s.chars().next().unwrap() {
            'U' => Complex::new(0, 1),
            'D' => Complex::new(0, -1),
            'L' => Complex::new(-1, 0),
            'R' => Complex::new(1, 0),
            _ => panic!("Wat!"),
        },
        length: s[1..].parse().unwrap()
    }
}

fn input() -> impl Iterator<Item=Vec<Segment>> {
    let reader = io::BufReader::new(match env::args().nth(1) {
        None => unsafe {fs::File::from_raw_fd(0)},
        Some(filename) => fs::File::open(filename).unwrap(),
    });
    reader.lines()
        .map(|l| l.unwrap()
             .split(',')
             .map(read_segment)
             .collect())
}

fn main() {
    let mut iter = input();

    let mut wire1 = HashMap::new();
    let mut position: Complex<i32> = Complex::new(0, 0);
    let mut steps = 0;
    for rec in iter.next().unwrap() {
        for _ in 0..rec.length {
            position += rec.direction;
            steps += 1;
            if !wire1.contains_key(&position) {
                wire1.insert(position, steps);
            }
        }
    }

    let mut least_steps = 9999999;
    let mut closest = 9999999;
    position = Complex::new(0, 0);
    steps = 0;
    for rec in iter.next().unwrap() {
        for _ in 0..rec.length {
            position += rec.direction;
            steps += 1;
            if wire1.contains_key(&position) {
                closest = cmp::min(closest, position.re.abs() + position.im.abs());
                least_steps = cmp::min(least_steps, steps + wire1.get(&position).unwrap());
            }
        }
    }
    println!("Result: {}", closest);
    println!("Result2: {}", least_steps);
}
