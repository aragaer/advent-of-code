use anyhow::Result;
use getopts::Options;
use std::env;
use std::io::{BufRead, BufReader};
use std::iter::successors;
use std::fs;

#[derive(Debug)]
enum Shuffle {
    NewStack,
    Increment(isize),
    Cut(isize),
}

fn read_shuffle(filename: &str) -> impl Iterator<Item=Shuffle> {
    BufReader::new(fs::File::open(filename).unwrap())
        .lines()
        .map(|line| {
            let line = line.unwrap();
            let words: Vec<_> = line.split(' ').collect();
            if words[0] == "cut" {
                Shuffle::Cut(words[1].parse().unwrap())
            } else if words[1] == "into" {
                Shuffle::NewStack
            } else {
                Shuffle::Increment(words[3].parse().unwrap())
            }
        })
}

// Copied from Rosetta code
fn mod_inv(a: i128, module: i128) -> i128 {
    let mut mn = (module, a);
    let mut xy = (0, 1);

    while mn.1 != 0 {
        xy = (xy.1, xy.0 - (mn.0 / mn.1) * xy.1);
        mn = (mn.1, mn.0 % mn.1);
    }

    while xy.0 < 0 {
        xy.0 += module;
    }
    xy.0
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.optopt("s", "size", "", "");
    let matches = opts.parse(&args[1..])?;

    let size = match matches.opt_str("s") {
        None => 10007,
        Some(s) => s.parse::<usize>()?,
    };

    let shuffle_sequence: Vec<_> = read_shuffle(matches.free.get(0).unwrap())
        .collect();

    let mut cards: Vec<_> = (0..size).collect();
    for shuffle in shuffle_sequence.iter() {
        match *shuffle {
            Shuffle::NewStack => {
                cards.reverse();
            },
            Shuffle::Cut(x) if x > 0 => {
                cards.rotate_left(x as usize);
            },
            Shuffle::Cut(x) => {
                cards.rotate_right(-x as usize);
            },
            Shuffle::Increment(x) => {
                let x = x as usize;
                for (i, c) in cards.clone().iter().enumerate() {
                    cards[(i*x) % size] = *c;
                }
            },
        }
    }
    if size == 10007 {
        println!("Result: {}",
                 cards.iter().position(|&x| x == 2019).unwrap())
    } else {
        println!("{}", cards.iter()
                 .map(|n| n.to_string())
                 .collect::<Vec<String>>()
                 .join(" "));
    }

    let size = 119315717514047_i128;
    let times = 101741582076661_u64;

    let (mut a, mut b) = shuffle_sequence.iter()
        .fold((1_i128, 0_i128),
              |(a, b), shuffle|
              match *shuffle {
                  Shuffle::NewStack => ((size - a) % size, (size - b - 1) % size),
                  Shuffle::Cut(x) => (a, (b + size - x as i128) % size),
                  Shuffle::Increment(x) => ((a * x as i128) % size, (b * x as i128) % size),
              });
    #[cfg(debug_assertions)]
    println!("f(X) = {}X + {}", a, b);

    let (mut na, mut nb) = (1_i128, 0_i128);
    for bit in successors(Some(times),
                          |&n| if n > 1 {
                              Some(n >> 1)
                          } else {
                              None
                          })
        .map(|bit| bit & 1 != 0) {
            if bit {
                na = (na * a) % size;
                nb = (nb * a + b) % size;
            }
            b = (b * a + b) % size;
            a = (a * a) % size;
        }
    #[cfg(debug_assertions)]
    println!("f^{}(X) = {}X + {}", times, na, nb);
    let inv_a = mod_inv(na, size);
    #[cfg(debug_assertions)]
    println!("f^-{}(X) = {}X + {}", times, inv_a, ((size-nb) * inv_a) % size);
    let result2 = (inv_a * 2020_i128 + (size-nb) * inv_a) % size;
    #[cfg(debug_assertions)]
    println!("f^-{}(2020) = {}", times, result2);
    println!("Result2: {}", result2);

    Ok(())
}
