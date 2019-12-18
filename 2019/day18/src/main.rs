use anyhow::Result;
use getopts::Options;
use itertools::Itertools;
use num::complex::Complex;
use std::cmp::{max, min};
use std::env;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{Read, BufReader};

struct Maze {
    width: isize,
    height: isize,
    map: HashMap<Complex<isize>, char>,
    keys: HashMap<char, Complex<isize>>,
    doors: HashMap<char, Complex<isize>>,
    entrance: Complex<isize>,
    distances: HashMap<(char, char), (usize, HashSet<char>)>
}

impl Maze {
    fn from(reader: &mut dyn Read) -> Maze {
        let mut map = HashMap::new();
        let mut x = 0;
        let mut y = 0;
        let mut width = 0;
        let mut keys = HashMap::new();
        let mut doors = HashMap::new();
        let mut entrance = Complex::new(0, 0);
        for c in reader.bytes().map(|b| b.unwrap() as char) {
            if c == '\n' {
                y += 1;
                if x == 0 {
                    break;
                }
                width = max(x, width);
                x = 0;
            } else {
                let pos = Complex::new(x, y);
                match c {
                    'a'..='z' => {
                        keys.insert(c, pos.clone());
                    },
                    'A'..='Z' => {
                        doors.insert(c, pos.clone());
                    },
                    '@' => {
                        entrance = pos.clone();
                    },
                    _  => {}
                };
                map.insert(pos, c);
                x += 1;
            }
        }

        Maze {
            width,
            height: y+1,
            map,
            keys,
            doors,
            entrance,
            distances: HashMap::new(),
        }
    }

    fn load(filename: &String) -> Maze {
        Maze::from(&mut BufReader::new(fs::File::open(filename).unwrap()))
    }

    fn way(&mut self, from: Complex<isize>, to: Complex<isize>) -> (usize, HashSet<char>) {
        let c_from = *self.map.get(&from).unwrap();
        let c_to = *self.map.get(&to).unwrap();
        if self.distances.contains_key(&(c_from, c_to)) {
            return self.distances.get(&(c_from, c_to)).unwrap().clone();
        }
        let mut queue: Vec<(_, usize, HashSet<char>)> = vec![(from, 0, HashSet::new())];
        let mut visited = HashSet::new();
        loop {
            let (pos, dist, found) = queue.remove(0);
            visited.insert(pos);
            for &neigh in &[pos+1, pos-1, pos+Complex::i(), pos-Complex::i()] {
                if neigh == to {
                    self.distances.insert((c_from, c_to), (dist+1, found.clone()));
                    self.distances.insert((c_to, c_from), (dist+1, found.clone()));
                    return (dist+1, found);
                }
                if visited.contains(&neigh) {
                    continue;
                }
                let ch = *self.map.get(&neigh).unwrap();
                if ch == '#' {
                    continue;
                }
                let mut found = found.clone();
                if ch.is_ascii_lowercase() {
                    found.insert(ch);
                }
                queue.push((neigh, dist+1, found));
            }
        }
    }

    fn flood_scan(&mut self) -> HashMap<char, HashSet<char>> {
        let mut result = HashMap::new();
        let mut queue = vec![(self.entrance, 0, HashSet::new())];
        let mut visited = HashSet::new();
        while !queue.is_empty() {
            let (pos, dist, reqs) = queue.remove(0);
            visited.insert(pos);
            for &neigh in &[pos+1, pos-1, pos+Complex::i(), pos-Complex::i()] {
                if visited.contains(&neigh) {
                    continue;
                }
                let mut my_reqs = reqs.clone();
                let c = *self.map.get(&neigh).unwrap_or(&' ');
                match c {
                    '#' => {
                        continue;
                    },
                    'a'..='z' => {
                        result.insert(c, my_reqs.clone());
                    },
                    'A'..='Z' => {
                        my_reqs.insert(c.to_ascii_lowercase());
                    },
                    _ => {},
                }
                queue.push((neigh, dist+1, my_reqs));
            }
        }
        result
    }
}

fn collect_keys(maze: &mut Maze,
                loc: Complex<isize>,
                key_reqs: &HashMap<char, HashSet<char>>,
                keys: HashSet<char>) -> usize {
    let mut best = std::i32::MAX as usize;
    let mut options: Vec<(Complex<isize>, usize, HashSet<char>)> = vec![(loc, 0, HashSet::new())];
    while !options.is_empty() {
        let (pos, result, found) = options.pop().unwrap();
        if result >= best {
            continue;
        }
        if key_reqs.len() == found.len() {
            best = result;
            dbg!(best);
        }
        let mut n_options: Vec<_> = key_reqs.iter()
            .filter_map(|(key, reqs)| {
                if found.contains(&key) {
                    return None;
                }
                if reqs.difference(&found).count() > 0 {
                    return None;
                }
                let k_pos = *maze.keys.get(&key).unwrap();
                let (dist, w_found) = maze.way(pos, k_pos);
                if w_found.difference(&found).count() > 0 {
                    return None;
                }
                if result+dist >= best {
                    return None;
                }
                let mut n_found = found.clone();
                n_found.insert(*key);
                Some((k_pos, result+dist, n_found))
            })
            .collect();
        n_options.sort_by_key(|t| -(t.1 as isize));
        options.extend(n_options);
    }
    best
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let opts = Options::new();
    let matches = opts.parse(&args[1..])?;

    let mut maze = Maze::load(&matches.free[0]);

    #[cfg(debug_assertions)]
    for y in 0..maze.height {
        let s: String = (0..maze.width)
            .map(|x| maze.map.get(&Complex::new(x, y)).unwrap_or(&'?'))
            .collect();
        println!("{}", s);
    }

    let key_reqs = maze.flood_scan();

    #[cfg(debug_assertions)]
    for (key, reqs) in key_reqs.iter() {
        println!("{}: {}", key, reqs.iter().cloned()
                 .intersperse(',')
                 .collect::<String>());
    }

    let start = maze.entrance.clone();
    let result = collect_keys(&mut maze, start, &key_reqs, HashSet::new());
    println!("Result: {}", result);
    Ok(())
}
