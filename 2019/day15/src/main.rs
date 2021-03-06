#[macro_use]
extern crate num_derive;

use anyhow::Result;
use getopts::Options;
use num_traits::cast::ToPrimitive;
use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use std::env;
use std::iter::successors;

mod intcode;

use crate::intcode::{Intcode, Program};

#[derive(ToPrimitive, Clone, Copy)]
enum Direction {
    North = 1,
    South = 2,
    West = 3,
    East = 4,
}

static ALL_DIRECTIONS: [Direction; 4] = [Direction::North, Direction::South, Direction::East, Direction::West];

fn position_at(position: (i32, i32), direction: Direction) -> (i32, i32) {
    match direction {
        Direction::North => (position.0, position.1 - 1),
        Direction::South => (position.0, position.1 + 1),
        Direction::West => (position.0 + 1, position.1),
        Direction::East => (position.0 - 1, position.1),
    }
}

struct Maze {
    code: Intcode,
    left: i32,
    right: i32,
    top: i32,
    bottom: i32,
    droid: (i32, i32),
    map: HashMap<(i32, i32), char>,
    depth: char,
    oxygen: Option<(i32, i32)>,
}

impl Maze {
    fn new(code: Intcode) -> Maze {
        let mut result = Maze {
            code: code,
            left: -3,
            right: 3,
            top: -3,
            bottom: 3,
            droid: (0, 0),
            map: HashMap::new(),
            depth: 'a',
            oxygen: None,
        };
        result.map.insert((0, 0), '.');
        result
    }

    fn draw(&self) {
        for y in self.top..=self.bottom {
            let s: String = (self.left..=self.right)
                .map(|x| if (x, y) == self.droid {
                    'D'
                } else {
                    *self.map.get(&(x, y)).unwrap_or(&' ')
                }).collect();
            println!("{}", s);
        }
    }

    fn go(&mut self, movement: Direction) {
        let new_position = position_at(self.droid, movement);
        self.bottom = max(self.bottom, new_position.1+1);
        self.top = min(self.top, new_position.1);
        self.left = min(self.left, new_position.0);
        self.right = max(self.right, new_position.0+1);
        self.code.input.clear();
        self.code.input.push(movement.to_i64().unwrap());
        match self.code.next().unwrap() {
            0 => {
                self.map.insert(new_position, '#');
            },
            1 => {
                self.map.insert(new_position, '.');
                self.droid = new_position;
            },
            2 => {
                self.map.insert(new_position, '.');
                self.droid = new_position;
                self.oxygen = Some(self.droid);
            },
            x => {
                panic!("Got {}", x);
            }
        }
    }

    fn check(&mut self, movement: Direction) -> char {
        let new_position = position_at(self.droid, movement);
        if self.map.contains_key(&new_position) {
            return self.map.get(&new_position).unwrap().clone();
        }
        self.go(movement);
        if self.droid == new_position {
            self.go(match movement {
                Direction::North => Direction::South,
                Direction::South => Direction::North,
                Direction::East => Direction::West,
                Direction::West => Direction::East,
            });
        }
        return self.map.get(&new_position).unwrap().clone();
    }

    fn backtrack(&mut self) {
        loop {
            self.map.insert(self.droid, 'Z');
            for movement in &ALL_DIRECTIONS {
                let tile = self.check(*movement);
                if tile == self.depth {
                    self.go(*movement);
                    break;
                }
                if tile == '?' {
                    self.go(*movement);
                    self.map.insert(self.droid, '.');
                    self.depth = (self.depth as u8 - 1) as char;
                    return;
                }
            }
        }
    }

    fn explore(&mut self, max_steps: i32) {
        let mut oxygen_found = false;
        for _ in 0..max_steps {
            let mut options = ALL_DIRECTIONS.iter()
                .map(|&d| d)
                .filter(|&d| self.check(d) == '.');
            match options.next() {
                None => {
                    if self.depth == 'a' {
                        break;
                    }
                    self.backtrack();
                },
                Some(direction) => {
                    if options.count() == 0 {
                        self.map.insert(self.droid, self.depth);
                    } else {
                        self.map.insert(self.droid, '?');
                        self.depth = (self.depth as u8 + 1) as char;
                    }
                    self.go(direction);
                },
            }
            if self.oxygen != None && !oxygen_found {
                oxygen_found = true;
                let result = self.map.values()
                    .filter(|&&c| c.is_ascii_lowercase() || c == '?')
                    .count();
                println!("Result: {}", result);
            }
        }
    }

    fn flood_step(&mut self, sources: &HashSet<(i32, i32)>) -> Option<HashSet<(i32, i32)>> {
        let mut new_sources = HashSet::new();
        for &source in sources.iter() {
            for &direction in ALL_DIRECTIONS.iter() {
                let neighbour = position_at(source, direction);
                let obj = *self.map.get(&neighbour).unwrap();
                if obj != 'O' && obj != '#' {
                    self.map.insert(neighbour.clone(), 'O');
                    new_sources.insert(neighbour);
                }
            }
        }
        if new_sources.is_empty() {
            None
        } else {
            Some(new_sources)
        }
    }

    fn flood_oxygen(&mut self) -> i32 {
        let mut sources = HashSet::new();
        self.map.insert(self.oxygen.unwrap(), 'O');
        sources.insert(self.oxygen.unwrap());
        successors(Some(sources),
                   |s| self.flood_step(s))
            .count() as i32 -1
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.optopt("s", "", "", "");
    let matches = opts.parse(&args[1..])?;
    let program = Program::load(matches.free.iter().next());
    let code = Intcode::new(&program, vec![]);
    let mut maze = Maze::new(code);
    let max_steps = match matches.opt_str("s") {
        None => std::i32::MAX,
        Some(s) => s.parse::<i32>()?,
    };
    maze.explore(max_steps);
    /*
    println!("------------------");
    maze.draw();
    println!("Droid at {}:{}", maze.droid.0, maze.droid.1);
    */
    println!("Result2: {}", maze.flood_oxygen());

    Ok(())
}
