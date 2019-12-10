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

fn direction(target: &Complex<i32>) -> f32 {
    // Return -PI for "up" so that it goes first when sorting
    if target.re == 0 && target.im < 0 {
        return -std::f32::consts::PI;
    }
    let direction: Complex<f32> = Complex::new(target.re as f32, target.im as f32);
    (-direction*Complex::i()).to_polar().1
}

fn square_mod(target: Complex<i32>) -> i32 {
    target.re * target.re + target.im * target.im
}

#[derive(Eq, PartialEq)]
struct Dir {
    inner: Complex<i32>,
}

impl From<Complex<i32>> for Dir {
    fn from(complex: Complex<i32>) -> Self {
        Dir{inner: complex / gcd(complex.re.abs(), complex.im.abs())}
    }
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

fn directions_around(point: &Complex<i32>, asteroids: &Vec<Complex<i32>>) -> BTreeMap<Dir, Vec<Complex<i32>>> {
    let mut result = BTreeMap::new();
    for asteroid in asteroids.into_iter() {
        if asteroid != point {
            result.entry(Dir::from(asteroid-point))
                .or_insert(Vec::new())
                .push(*asteroid);
        }
    }
    result.values_mut()
        .for_each(|v|
                  v.sort_unstable_by_key(|a|
                                         square_mod(a-point)));
    result
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.optopt("p", "position", "", "");
    let matches = opts.parse(&args[1..])?;

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

    let mut count = cmp::min(200, asteroids.len());
    while count > 0 {
        for targets in a_d.values_mut() {
            if targets.is_empty() {
                continue;
            }
            let shot = targets.remove(0);
            // println!("shot {}", shot);
            count -= 1;
            if count == 0 {
                let result2 = shot.re * 100 + shot.im;
                println!("Result2: {}", result2);
                break;
            }
        }
    }
    Ok(())
}
