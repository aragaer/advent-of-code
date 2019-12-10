extern crate anyhow;
extern crate getopts;
extern crate itertools;
extern crate num;

use anyhow::Result;
use getopts::Options;
use itertools::Itertools;
use std::cmp;
use std::cmp::Ordering;
use num::complex::Complex;
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::os::unix::io::FromRawFd;

fn input(args: &Vec<String>) -> impl Iterator<Item=Vec<bool>> {
    io::BufReader::new(match args.iter().next() {
        None => unsafe {fs::File::from_raw_fd(0)},
        Some(filename) => fs::File::open(filename).unwrap(),
    }).lines()
        .map(|l| l.unwrap().chars()
             .map(|c| c == '#')
             .collect())
}

fn gcd(mut a: i32, mut b: i32) -> i32 {
    while b != 0 {
        let t = a;
        a = b;
        b = t % b;
    }
    a
}

fn visible(from: &Complex<i32>, to: &Complex<i32>, map: &Vec<Vec<bool>>) -> bool {
    let diff = to - from;
    let steps = gcd(diff.re.abs(), diff.im.abs());
    let step = diff / steps;

    let mut pos = from.clone();
    for _ in 1..steps {
        pos += step;
        if map[pos.im as usize][pos.re as usize] {
            return false;
        }
    }
    true
}

fn direction(target: &Complex<i32>) -> f32 {
    // Return -PI for "up" so that it goes first when sorting
    if target.re == 0 && target.im < 0 {
        return -std::f32::consts::PI;
    }
    let direction: Complex<f32> = Complex::new(target.re as f32, target.im as f32);
    (-direction*Complex::i()).to_polar().1
}

fn integer_distance(target: &Complex<i32>) -> i32 {
    target.re * target.re + target.im * target.im
}

#[derive(Eq, PartialEq)]
struct Dir {
    inner: Complex<i32>,
}

impl Ord for Dir {
    fn cmp(&self, other: &Self) -> Ordering {
        let d1 = direction(&self.inner);
        let d2 = direction(&other.inner);
        d1.partial_cmp(&d2).unwrap()
    }
}

impl PartialOrd for Dir {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn integer_direction(point: &Complex<i32>) -> Dir {
    Dir{inner: point / gcd(point.re.abs(), point.im.abs())}
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.optopt("p", "position", "", "");
    let matches = opts.parse(&args[1..])?;

    let field: Vec<Vec<bool>> = input(&matches.free).collect();
    let mut asteroids: Vec<Complex<i32>> = Vec::new();
    for (i, l) in field.iter().enumerate() {
        for (j, p) in l.iter().enumerate() {
            if *p {
                asteroids.push(Complex::new(j as i32, i as i32));
            }
        }
    }

    let station;

    if matches.opt_present("p") {
        let (x, y) = matches.opt_str("p")
            .expect("position")
            .split(':')
            .map(|s| s.parse::<i32>().unwrap())
            .collect_tuple()
            .expect("tuple");
        station = Complex::new(x, y);
        if asteroids.contains(&station) {
            asteroids = asteroids.into_iter()
                .filter(|x| *x != station)
                .collect();
        }
    } else {
        let result = asteroids.iter()
            .map(|a| asteroids.iter()
                 .filter(|b| *b != a && visible(&a, b, &field))
                 .count())
            .enumerate()
            .max_by_key(|(_,c)| c.clone())
            .expect("no asteroids?");
        println!("Result: {}", result.1);
        station = asteroids[result.0];
        asteroids.remove(result.0);
    }

    let mut count = cmp::min(200, asteroids.len());
    let mut a_d: BTreeMap<_, Vec<_>> = BTreeMap::new();

    for asteroid in asteroids.into_iter() {
        let dir = integer_direction(&(asteroid-station));
        a_d.entry(dir).or_insert(Vec::new()).push(asteroid);
    }

    for v in a_d.values_mut() {
        v.sort_unstable_by_key(|a| integer_distance(&(a-station)));
    }

    let result2;
    'outer: loop {
        for targets in a_d.values_mut() {
            if targets.is_empty() {
                continue;
            }
            let shot = targets.remove(0);
            // println!("shot {}", shot);
            count -= 1;
            if count == 0 {
                result2 = shot.re * 100 + shot.im;
                break 'outer;
            }
        }
    }
    println!("Result2: {}", result2);
    Ok(())
}
