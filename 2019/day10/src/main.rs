extern crate getopts;
extern crate itertools;
extern crate num;

use getopts::Options;
use itertools::Itertools;
use num::complex::Complex;
use num::integer::gcd;
use std::cmp::{min, Ordering};
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::io::{BufRead, BufReader};
use std::os::unix::io::FromRawFd;

fn input(args: &Vec<String>) -> impl Iterator<Item=Vec<bool>> {
    BufReader::new(match args.iter().next() {
        None => unsafe {fs::File::from_raw_fd(0)},
        Some(filename) => fs::File::open(filename).unwrap(),
    }).lines()
        .map(|l| l.unwrap().chars()
             .map(|c| c == '#')
             .collect())
}

#[derive(Eq, PartialEq)]
struct Dir {
    inner: Complex<i32>,
}

impl Dir {
    fn dir(&self) -> f32 {
        // Return -PI for -i so that it goes first when sorting
        let (re, im) = (self.inner.re, self.inner.im);
        if re == 0 && im < 0 {
            return -std::f32::consts::PI;
        }
        // Rotate by -PI/2
        Complex::new(im as f32, -re as f32).arg()
    }
}

impl From<Complex<i32>> for Dir {
    fn from(complex: Complex<i32>) -> Self {
        Dir{inner: complex / gcd(complex.re, complex.im)}
    }
}

impl Ord for Dir {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(&other).unwrap()
    }
}

impl PartialOrd for Dir {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.dir().partial_cmp(&other.dir())
    }
}

fn directions_around(point: &Complex<i32>, asteroids: &Vec<Complex<i32>>) -> BTreeMap<Dir, Vec<Complex<i32>>> {
    let mut result = BTreeMap::new();
    for asteroid in asteroids.iter()
        .filter(|a| *a != point) {
            result.entry(Dir::from(asteroid-point))
                .or_insert(Vec::new())
                .push(*asteroid);
        }
    result.values_mut()
        .for_each(|v|
                  v.sort_unstable_by_key(|a|
                                         (a-point).norm_sqr()));
    result
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.optopt("p", "position", "", "");
    let matches = opts.parse(&args[1..]).expect("parse args");

    let field: Vec<_> = input(&matches.free).collect();
    let asteroids: Vec<_> = field.iter().enumerate()
        .flat_map(|(li, l)| l.iter()
                  .enumerate()
                  .filter(|p| *p.1)
                  .map(move |s| (li, s.0)))
        .map(|(i, j)| Complex::new(j as i32, i as i32))
        .collect();

    let mut a_d = if matches.opt_present("p") {
        let (x, y) = matches.opt_str("p")
            .expect("position")
            .split(':')
            .map(|s| s.parse::<i32>().unwrap())
            .collect_tuple()
            .expect("tuple");
        directions_around(&Complex::new(x, y), &asteroids)
    } else {
        asteroids.iter()
            .map(|a| directions_around(&a, &asteroids))
            .max_by_key(|m| m.len())
            .expect("no asteroids?")
    };
    println!("Result: {}", a_d.len());

    let mut count = min(200, asteroids.len());
    let result2 = loop {
        let (i, shot) = a_d.values_mut()
            .map(|t| t.remove(0))
            .take(count)
            .enumerate()
            .last()
            .expect("take last");
        count -= i+1;
        if count == 0 {
            break shot.re * 100 + shot.im;
        }
    };
    println!("Result2: {}", result2);
}
