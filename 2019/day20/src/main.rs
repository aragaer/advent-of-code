use num::complex::Complex;
use std::cmp::max;
use std::env;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::fs::File;
use std::io::{Read, BufReader};

struct Maze {
    width: isize,
    height: isize,
    map: HashMap<Complex<isize>, char>,
    portal_links: HashMap<Complex<isize>, Complex<isize>>,
    #[cfg(debug_assertions)]
    portal_names: HashMap<Complex<isize>, String>,
    entrance: Complex<isize>,
    exit: Complex<isize>,
}

fn around(pos: Complex<isize>) -> [Complex<isize>; 4] {
    [pos+1,
     pos-1,
     pos+Complex::i(),
     pos-Complex::i()]
}

impl Maze {
    fn from(reader: &mut dyn Read) -> Maze {
        let mut map = HashMap::new();
        let mut x = 0;
        let mut y = 0;
        let mut width = 0;
        #[cfg(debug_assertions)]
        let mut portal_names = HashMap::new();
        let mut portal_links = HashMap::new();
        let mut seen_portal_chars = Vec::new();
        let mut seen_portals: HashMap<String, Complex<isize>> = HashMap::new();
        for c in reader.bytes().map(|b| b.unwrap() as char) {
            if c == '\n' {
                y += 1;
                width = max(x, width);
                x = 0;
            } else {
                let pos = Complex::new(x, y);
                if c.is_ascii_uppercase() {
                    seen_portal_chars.push(pos);
                }
                map.insert(pos, c);
                x += 1;
            }
        }

        for portal_char_pos in seen_portal_chars.iter() {
            let mut got_floor = false;
            let this_char = *map.get(portal_char_pos).unwrap();
            let mut portal_name = String::new();
            for n in &around(*portal_char_pos) {
                let n_ch = *map.get(n).unwrap_or(&' ');
                got_floor = got_floor || n_ch == '.';
                if n_ch.is_ascii_uppercase() {
                    portal_name = if ((portal_char_pos - n).re + (portal_char_pos - n).im) == 1 {
                        n_ch.to_string() + &this_char.to_string()
                    } else {
                        this_char.to_string() + &n_ch.to_string()
                    };
                }
            }
            if !got_floor {
                continue;
            }
            #[cfg(debug_assertions)]
            portal_names.insert(*portal_char_pos, portal_name.clone());
            match seen_portals.entry(portal_name) {
                Occupied(o) => {
                    let other = *o.get();
                    portal_links.insert(portal_char_pos.clone(), other);
                    portal_links.insert(other, *portal_char_pos);
                },
                Vacant(v) => {
                    v.insert(*portal_char_pos);
                },
            }
        }

        Maze {
            width,
            height: y+1,
            map,
            portal_links,
            #[cfg(debug_assertions)]
            portal_names,
            entrance: *seen_portals.get("AA").unwrap(),
            exit: *seen_portals.get("ZZ").unwrap(),
        }
    }

    fn load(filename: &str) -> Maze {
        Maze::from(&mut BufReader::new(File::open(filename).unwrap()))
    }

    fn is_outside(&self, pos: Complex<isize>) -> bool {
        pos.re < 2 || pos.re > self.width - 3
            || pos.im < 2 || pos.im > self.height - 3
    }

    fn go_to_exit(&self, recursive: bool) -> i32 {
        let mut visited = HashSet::new();
        let mut queue = vec![(self.entrance, 0, -1)];
        while !queue.is_empty() {
            let (pos, depth, dist) = queue.remove(0);
            visited.insert((pos, depth));
            for &neigh in &around(pos) {
                if neigh == self.exit && depth == 0 {
                    return dist;
                }
                if visited.contains(&(neigh, depth)) {
                    continue;
                }
                if self.portal_links.contains_key(&neigh) {
                    let new_depth = if !recursive {
                        0
                    } else if self.is_outside(neigh) {
                        depth - 1
                    } else {
                        depth + 1
                    };
                    if new_depth >= 0 {
                        let to = *self.portal_links.get(&neigh).unwrap();
                        if visited.contains(&(to, new_depth)) {
                            continue;
                        }
                        #[cfg(debug_assertions)]
                        println!("Teleport from {} to {} on step {} goes to depth {}->{}", pos, &to, dist, depth, new_depth);
                        queue.push((to, new_depth, dist));
                    }
                } else if self.map.get(&neigh) == Some(&'.') && dist < 10000 {
                    queue.push((neigh, depth, dist+1));
                }
            }
            // stick to outer levels -- speeds things up a lot
            queue.sort_unstable_by_key(|t| t.1);
        }
        panic!("no exit");
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let maze = Maze::load(&args[1]);

    #[cfg(debug_assertions)]
    for y in 0..maze.height {
        println!("{}", (0..maze.width)
                 .map(|x| maze.map.get(&Complex::new(x, y)).unwrap_or(&'?'))
                 .collect::<String>());
    }
    #[cfg(debug_assertions)]
    for (from, to) in maze.portal_links.iter() {
        println!("{} {} -> {} goes {}",
                 maze.portal_names.get(&from).unwrap(),
                 from, to, if maze.is_outside(*from) {
                     "outside"
                 } else {
                     "inside"
                 });
    }
    println!("Result: {}", maze.go_to_exit(false));
    println!("Result2: {}", maze.go_to_exit(true));
}
