use anyhow::Result;
use getopts::Options;
#[cfg(debug_assertions)]
use itertools::Itertools;
use num::complex::Complex;
use std::cmp::max;
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

    fn way(&mut self, from: Complex<isize>, to: Complex<isize>, stop_at: usize) -> Option<(usize, HashSet<char>)> {
        let c_from = *self.map.get(&from).unwrap();
        let c_to = *self.map.get(&to).unwrap();
        if self.distances.contains_key(&(c_from, c_to)) {
            return Some(self.distances.get(&(c_from, c_to)).unwrap().clone());
        }
        let mut queue: Vec<(_, usize, HashSet<char>)> = vec![(from, 0, HashSet::new())];
        let mut visited = HashSet::new();
        loop {
            let (pos, dist, found) = queue.remove(0);
            if stop_at == dist {
                return None;
            }
            visited.insert(pos);
            for &neigh in &[pos+1, pos-1, pos+Complex::i(), pos-Complex::i()] {
                if neigh == to {
                    self.distances.insert((c_from, c_to), (dist+1, found.clone()));
                    self.distances.insert((c_to, c_from), (dist+1, found.clone()));
                    return Some((dist+1, found));
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

    fn flood_scan(&mut self) -> (HashMap<char, HashSet<char>>, HashMap<char, usize>) {
        let mut result = HashMap::new();
        let mut reachable_from = HashMap::new();
        let mut visited = HashSet::new();
        self.map.insert(self.entrance+1, '#');
        self.map.insert(self.entrance-1, '#');
        self.map.insert(self.entrance+Complex::i(), '#');
        self.map.insert(self.entrance-Complex::i(), '#');
        for (i, entrance) in [Complex::new(-1,-1), Complex::new(1,-1),
                              Complex::new(-1,1), Complex::new(1,1)].iter()
                             .map(|o| self.entrance+o)
                             .enumerate() {
                let mut queue = vec![(entrance, HashSet::new())];
                while !queue.is_empty() {
                    let (pos, reqs) = queue.remove(0);
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
                                reachable_from.insert(c, i);
                            },
                            'A'..='Z' => {
                                my_reqs.insert(c.to_ascii_lowercase());
                            },
                            _ => {},
                        }
                        queue.push((neigh, my_reqs));
                    }
                }
            }
        (result, reachable_from)
    }
}

fn collect_keys(maze: &mut Maze,
                locs: Vec<Complex<isize>>,
                key_reqs: &HashMap<char, HashSet<char>>,
                reachable_from: &HashMap<char, usize>) -> usize {
    let mut best = std::i32::MAX as usize;
    let mut options: Vec<(Vec<Complex<isize>>, usize, HashSet<char>)> = vec![(locs, 0, HashSet::new())];
    let total_keys = key_reqs.len();
    dbg!(total_keys);
    while !options.is_empty() {
        let (locs, result, found) = options.pop().unwrap();
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
                let ent = *reachable_from.get(&key).unwrap();
                let k_pos = *maze.keys.get(&key).unwrap();
                let (dist, w_found) = maze.way(locs[ent], k_pos, result-best)?;
                if w_found.difference(&found).count() > 0 {
                    return None;
                }
                if result+dist+total_keys-found.len() >= best {
                    return None;
                }
                let mut n_found = found.clone();
                n_found.insert(*key);
                let mut n_locs = locs.clone();
                n_locs[ent] = k_pos;
                Some((n_locs, result+dist, n_found))
            })
            .collect();
        n_options.sort_by_key(|t| -(t.1 as isize));
        options.extend(n_options);
        // println!("{}, {}", options.len(), found.len()+1);
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

    let (key_reqs, reachable_from) = maze.flood_scan();

    #[cfg(debug_assertions)]
    for (key, reqs) in key_reqs.iter() {
        println!("{}: requires: {}, reachable from {}", key, reqs.iter().cloned()
                 .intersperse(',')
                 .collect::<String>(),
                 reachable_from.get(key).unwrap());
    }

    let start = maze.entrance.clone();
    let result = collect_keys(&mut maze,
                              [Complex::new(-1,-1), Complex::new(1,-1),
                               Complex::new(-1,1), Complex::new(1,1)].iter()
                              .map(|o| o+start)
                              .collect(),
                              &key_reqs, &reachable_from);
    println!("Result: {}", result);
    Ok(())
}
