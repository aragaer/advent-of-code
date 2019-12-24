use itertools::Itertools;
use num::complex::Complex;
use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use std::env;
use std::io::{BufRead, BufReader};
use std::iter::successors;
use std::fs::File;

fn input() -> HashMap<Complex<i32>, bool> {
    let mut result = HashMap::new();
    let filename = env::args().nth(1).unwrap();
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    for (y, l) in reader.lines()
        .map(|l| l.unwrap())
        .enumerate() {
            for (x, c) in l.chars().enumerate() {
                result.insert(Complex::new(x as i32, y as i32), c == '#');
            }
        }
    result
}

fn biodiversity(data: &HashMap<Complex<i32>, bool>) -> i32 {
    (0..5)
        .flat_map(|y| (0..5).map(move |x| Complex::new(x, y)))
        .map(|c| data.get(&c).unwrap())
        .zip(successors(Some(1), |n| Some(n * 2)))
        .fold(0, |a, (b, c)| if *b { a + c } else { a })
}

fn count_neighs(map: &HashMap<Complex<i32>, bool>, pos: Complex<i32>) -> usize {
    [Complex::new(-1, 0), Complex::new(1, 0), Complex::i(), -Complex::i()].iter()
        .map(|o| pos + o)
        .map(|p| map.get(&p).unwrap_or(&false))
        .filter(|&&b| b)
        .count()
}

fn evolve(data: HashMap<Complex<i32>, bool>) -> HashMap<Complex<i32>, bool> {
    let mut result = HashMap::new();
    for pos in (0..5)
        .flat_map(|y| (0..5).map(move |x| Complex::new(x, y))) {
            let n = count_neighs(&data, pos);
            let bug = data.get(&pos).unwrap();
            let new_bug = (n == 1) || ((n == 2) && !bug);
            result.insert(pos, new_bug);
        }
    result
}

struct RecursiveMap {
    data: HashMap<(i32, i32, i32), bool>,
    min_level: i32,
    max_level: i32
}

impl RecursiveMap {
    fn from(flat: &HashMap<Complex<i32>, bool>) -> RecursiveMap {
        let mut data = HashMap::new();
        for (k, v) in flat.iter() {
            data.insert((k.re, k.im, 0), *v);
        }
        RecursiveMap {
            data,
            min_level: 0,
            max_level: 0,
        }
    }

    fn get(&self, x: i32, y: i32, z: i32) -> usize {
        *self.data.get(&(x, y, z)).unwrap_or(&false) as usize
    }

    fn count_neighs(&self, x: i32, y: i32, z: i32) -> usize {
        let mut result = 0;
        result += match x {
            0 => self.get(1, 2, z-1),
            3 => if y == 2 {
                (0..5).map(|y| self.get(4, y, z+1)).sum()
            } else {
                self.get(2, y, z)
            },
            _ => self.get(x-1, y, z),
            };
        result += match x {
            4 => self.get(3, 2, z-1),
            1 => if y == 2 {
                (0..5).map(|y| self.get(0, y, z+1)).sum()
            } else {
                self.get(2, y, z)
            },
            _ => self.get(x+1, y, z),
            };
        result += match y {
            0 => self.get(2, 1, z-1),
            3 => if x == 2 {
                (0..5).map(|x| self.get(x, 4, z+1)).sum()
            } else {
                self.get(x, 2, z)
            },
            _ => self.get(x, y-1, z),
            };
        result += match y {
            4 => self.get(2, 3, z-1),
            1 => if x == 2 {
                (0..5).map(|x| self.get(x, 0, z+1)).sum()
            } else {
                self.get(x, 2, z)
            },
            _ => self.get(x, y+1, z),
            };
        result
    }

    fn evolve(&mut self) {
        let mut new_data = HashMap::new();
        for z in (self.min_level-1)..=(self.max_level+1) {
            for (x, y) in (0..5).cartesian_product(0..5) {
                if x == 2 && y == 2 {
                    continue;
                }
                let pos = (x, y, z);
                let n = self.count_neighs(x, y, z);
                let bug = self.data.get(&pos).unwrap_or(&false);
                let new_bug = (n == 1) || ((n == 2) && !bug);
                if new_bug {
                    self.min_level = min(self.min_level, z);
                    self.max_level = max(self.max_level, z);
                }
                new_data.insert(pos, new_bug);
            }
        }
        self.data = new_data;
    }
}


fn main() {
    let mut map = input();
    let mut rec_map = RecursiveMap::from(&map);
    let mut seen = HashSet::new();
    let result = loop {
        let bd = biodiversity(&map);
        if seen.contains(&bd) {
            break bd;
        }
        seen.insert(bd);
        map = evolve(map);
    };
    println!("Result: {}", result);

    for _ in 0..200 {
        rec_map.evolve();
    }
    let result2 = rec_map.data.values()
        .filter(|&&b| b)
        .count();
    println!("Result2: {}", result2);
}
