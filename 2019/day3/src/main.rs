extern crate anyhow;
extern crate num;

use anyhow::Result;
use num::complex::Complex;
use std::cmp;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::env;
use std::fs;
use std::io::{BufRead, BufReader};
use std::os::unix::io::FromRawFd;

struct Segment {
    direction: Complex<i32>,
    length: u32,
}

impl TryFrom<&str> for Segment {
    type Error = anyhow::Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Ok(Segment {
            direction: match &s[0..1] {
                "U" => Complex::new(0, 1),
                "D" => Complex::new(0, -1),
                "L" => Complex::new(-1, 0),
                "R" => Complex::new(1, 0),
                _ => panic!("Wat!"),
            },
            length: s[1..].parse()?
        })
    }
}

fn input() -> impl Iterator<Item=Result<Vec<Segment>>> {
    BufReader::new(match env::args().nth(1) {
        None => unsafe {fs::File::from_raw_fd(0)},
        Some(filename) => fs::File::open(filename).unwrap(),
    }).lines().map(|l| l?.split(',')
                   .map(Segment::try_from)
                   .collect())
}

fn trace_wire<F>(wire: Result<Vec<Segment>>, mut f: F) -> Result<()> where
    F: FnMut(&Complex<i32>, i32) {
    let mut position: Complex<i32> = Complex::new(0, 0);
    let mut steps = 0;
    for segment in wire? {
        for _ in 0..segment.length {
            position += segment.direction;
            steps += 1;
            f(&position, steps);
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let mut wires = input();
    let mut wire1: HashMap<Complex<i32>, i32> = HashMap::new();
    let mut least_steps = std::i32::MAX;
    let mut closest = std::i32::MAX;

    trace_wire(wires.next().unwrap(),
               |position, steps| {
                   if !wire1.contains_key(position) {
                       wire1.insert(*position, steps);
                   }
               })?;

    trace_wire(wires.next().unwrap(),
               |position, steps| {
                   if wire1.contains_key(position) {
                       closest = cmp::min(closest, position.re.abs() + position.im.abs());
                       least_steps = cmp::min(least_steps, steps + wire1.get(position).unwrap());
                   }
               })?;

    println!("Result: {}", closest);
    println!("Result2: {}", least_steps);
    Ok(())
}
