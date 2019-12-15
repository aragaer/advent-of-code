use anyhow::Result;
use getopts::Options;
use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use std::env;

mod intcode;

use crate::intcode::{Intcode, Program};

struct Maze {
    left: i32,
    right: i32,
    top: i32,
    bottom: i32,
    droid: (i32, i32),
    map: HashMap<(i32, i32), char>,
    depth: char,
    oxygen: Option<(i32, i32)>
}

impl Maze {
    fn new() -> Maze {
        let mut result = Maze {
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
}


fn go(code: &mut Intcode, maze: &mut Maze, movement: i64) {
    let new_position = match movement {
        1 => (maze.droid.0, maze.droid.1 - 1),
        2 => (maze.droid.0, maze.droid.1 + 1),
        3 => (maze.droid.0 + 1, maze.droid.1),
        4 => (maze.droid.0 - 1, maze.droid.1),
        x => panic!("Incorrect movement direction {}", x),
    };
    maze.bottom = max(maze.bottom, new_position.1+1);
    maze.top = min(maze.top, new_position.1);
    maze.left = min(maze.left, new_position.0);
    maze.right = max(maze.right, new_position.0+1);
    code.input.clear();
    code.input.push(movement);
    match code.next().unwrap() {
        0 => {
            maze.map.insert(new_position, '#');
        },
        1 => {
            maze.map.insert(new_position, '.');
            maze.droid = new_position;
        },
        2 => {
            maze.map.insert(new_position, '.');
            maze.droid = new_position;
            maze.oxygen = Some(maze.droid);
        },
        x => {
            panic!("Got {}", x);
        }
    }
}

fn go_and_return(code: &mut Intcode, maze: &mut Maze, movement: i64) -> char {
    let new_position = match movement {
        1 => (maze.droid.0, maze.droid.1 - 1),
        2 => (maze.droid.0, maze.droid.1 + 1),
        3 => (maze.droid.0 + 1, maze.droid.1),
        4 => (maze.droid.0 - 1, maze.droid.1),
        x => panic!("Incorrect movement direction {}", x),
    };
    if maze.map.contains_key(&new_position) {
        return maze.map.get(&new_position).unwrap().clone();
    }
    go(code, maze, movement);
    if maze.droid == new_position {
        let back = match movement {
            1 => 2,
            2 => 1,
            3 => 4,
            4 => 3,
            _ => panic!("wat"),
        };
        go(code, maze, back);
    }
    return maze.map.get(&new_position).unwrap().clone();
}

fn look_around(code: &mut Intcode, maze: &mut Maze) -> Vec<i64> {
    let mut result = Vec::new();
    let mut ways = 0;
    for movement in 1..=4 {
        if go_and_return(code, maze, movement) == '.' {
            ways += 1;
            result.push(movement);
        }
    }

    if ways == 1 {
        maze.map.insert(maze.droid, maze.depth);
    }
    result
}

fn backtrack(code: &mut Intcode, maze: &mut Maze) {
    loop {
        maze.map.insert(maze.droid, 'Z');
        for movement in 1..=4 {
            let tile = go_and_return(code, maze, movement);
            if tile == maze.depth {
                go(code, maze, movement);
                break;
            }
            if tile == '?' {
                go(code, maze, movement);
                maze.map.insert(maze.droid, '.');
                maze.depth = (maze.depth as u8 - 1) as char;
                return;
            }
        }
    }
}

fn explore(code: &mut Intcode, maze: &mut Maze, max_steps: i32) -> Result<()> {
    let mut oxygen_found = false;
    for _ in 0..max_steps {
        let options = look_around(code, maze);
        match options.len() {
            0 => {
                if maze.depth == 'a' {
                    return Ok(());
                }
                backtrack(code, maze);
            },
            1 => {
                go(code, maze, options[0]);
            },
            _ => {
                maze.map.insert(maze.droid, '?');
                maze.depth = (maze.depth as u8 + 1) as char;
                go(code, maze, options[0]);
            }
        }
        if maze.oxygen != None && !oxygen_found {
            oxygen_found = true;
            let result = maze.map.values()
                .filter(|&&c| (c >= 'a' && c <= 'z') || c == '?')
                .count();
            println!("Result: {}", result);
        }
    }
    Ok(())
}


fn neighbours(pos: (i32, i32)) -> [(i32, i32); 4] {
    [(pos.0+1, pos.1),
     (pos.0-1, pos.1),
     (pos.0, pos.1+1),
     (pos.0, pos.1-1)]
}


fn flood_oxygen(maze: &mut Maze) -> i32 {
    let mut result = 0;
    let mut sources = HashSet::new();
    maze.map.insert(maze.oxygen.unwrap(), 'O');
    sources.insert(maze.oxygen.unwrap());
    loop {
        let mut new_sources = HashSet::new();
        for source in sources.drain() {
            for neighbour in neighbours(source).into_iter() {
                let obj = *maze.map.get(&neighbour).unwrap();
                if obj != 'O' && obj != '#' {
                    maze.map.insert(neighbour.clone(), 'O');
                    new_sources.insert(neighbour.clone());
                }
            }
        }
        if new_sources.is_empty() {
            break;
        }
        sources = new_sources;
        result += 1;
    }
    result
}


fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.optopt("m", "", "", "FILE");
    opts.optopt("s", "", "", "");
    let matches = opts.parse(&args[1..])?;
    let program = Program::load(matches.free.iter().next());
    let mut code = Intcode::new(&program, vec![]);
    let mut maze = Maze::new();
    let max_steps = match matches.opt_str("s") {
        None => std::i32::MAX,
        Some(s) => s.parse::<i32>()?,
    };
    explore(&mut code, &mut maze, max_steps)?;
    /*
    println!("------------------");
    maze.draw();
    println!("Droid at {}:{}", maze.droid.0, maze.droid.1);
    */
    let result2 = flood_oxygen(&mut maze);
    println!("Result2: {}", result2);

    Ok(())
}
