#[macro_use]
extern crate num_derive;

#[cfg(debug_assertions)]
use anyhow::anyhow;
use anyhow::Result;
use getopts::Options;
use itertools::Itertools;
use std::cmp::max;
use std::collections::HashMap;
use std::env;

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

#[cfg(debug_assertions)]
fn rotate(direction: Direction, turn: &str) -> Direction {
    if turn == "L" {
        match direction {
            Direction::North => Direction::West,
            Direction::West => Direction::South,
            Direction::South => Direction::East,
            Direction::East => Direction::North,
        }
    } else {
        match direction {
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
        }
    }
}

fn position_at(position: (isize, isize), direction: Direction) -> (isize, isize) {
    match direction {
        Direction::North => (position.0, position.1 - 1),
        Direction::South => (position.0, position.1 + 1),
        Direction::East => (position.0 + 1, position.1),
        Direction::West => (position.0 - 1, position.1),
    }
}

#[cfg(debug_assertions)]
struct Robot {
    pos: (isize, isize),
    dir: Direction,
}

struct Map {
    width: isize,
    height: isize,
    map: HashMap<(isize, isize), char>,
}

impl Map {
    fn from(code: &mut Intcode) -> Map {
        let mut map = HashMap::new();
        let mut x = 0;
        let mut y = 0;
        let mut width = 0;
        for c in code.map(|o| o as u8 as char) {
            if c == '\n' {
                y += 1;
                if x == 0 {
                    break;
                }
                width = max(x, width);
                x = 0;
            } else {
                map.insert((x, y), c);
                x += 1;
            }
        }

        Map {
            width,
            height: y-1,
            map,
        }
    }

    #[cfg(debug_assertions)]
    fn draw(&self) {
        for y in 0..self.height {
            let s: String = (0..self.width)
                .map(|x| *self.map.get(&(x, y)).unwrap_or(&' '))
                .collect();
            println!("{}", s);
        }
    }

    #[cfg(debug_assertions)]
    fn find_robot(&mut self) -> Robot {
        let pos = (0..self.width)
            .cartesian_product(0..self.height)
            .find(|pos| *self.map.get(pos).unwrap() == '^')
            .unwrap();
        self.map.insert(pos,'#');
        println!("Robot starts at {:?}", pos);
        Robot{pos, dir: Direction::North}
    }

    #[cfg(debug_assertions)]
    fn explore(&self, robot: &mut Robot) -> Vec<String> {
        let mut robot_direction = robot.dir;
        let mut pos = robot.pos;
        let mut result = Vec::new();

        if *self.map.get(&position_at(pos, robot_direction)).unwrap_or(&' ') != '#' {
            for turn in &["L", "R"] {
                let dir = rotate(robot_direction, turn);
                let forward = position_at(pos, dir);
                // println!("Looking {} to {:?}", turn, forward);
                if *self.map.get(&forward).unwrap_or(&' ') == '#' {
                    println!("{}", turn);
                    result.push(turn.to_string());
                    robot_direction = dir;
                    break;
                }
            }
        }

        loop {
            let mut count = 0;
            loop {
                let forward = position_at(pos, robot_direction);
                // println!("Looking forward to {:?}", forward);
                if *self.map.get(&forward).unwrap_or(&' ') != '#' {
                    break;
                }
                count += 1;
                pos = forward;
            }
            if count == 0 {
                println!("Dead end at {:?}", pos);
                break;
            }
            println!("{}", count);
            result.push(count.to_string());
            for turn in &["L", "R"] {
                let dir = rotate(robot_direction, turn);
                let forward = position_at(pos, dir);
                // println!("Looking {} to {:?}", turn, forward);
                if *self.map.get(&forward).unwrap_or(&' ') == '#' {
                    println!("{}", turn);
                    result.push(turn.to_string());
                    robot_direction = dir;
                    break;
                }
            }
        }
        result
    }
}

#[cfg(debug_assertions)]
fn step(robot: &mut Robot, map: &Map, command: &str) -> Result<()> {
    if command == "L" {
        robot.dir = rotate(robot.dir, "L");
    } else if command == "R" {
        robot.dir = rotate(robot.dir, "R");
    } else {
        for _ in 0..command.parse::<usize>().unwrap() {
            let forward = position_at(robot.pos, robot.dir);
            if *map.map.get(&forward).unwrap_or(&' ') != '#' {
                return Err(anyhow!("Robot fell off at {:?}", forward));
            }
            robot.pos = forward;
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let opts = Options::new();
    let matches = opts.parse(&args[1..])?;
    let mut program = Program::load(matches.free.iter().next());
    let map = Map::from(&mut Intcode::new(&program));
    #[cfg(debug_assertions)]
    let mut map = map;
    #[cfg(debug_assertions)]
    map.draw();

    let result = (0..map.width).cartesian_product(0..map.height)
        .filter(|pos| *map.map.get(pos).unwrap() == '#')
        .filter(|pos| ALL_DIRECTIONS.iter()
                .map(|&d| position_at(*pos, d))
                .map(|p| *map.map.get(&p).unwrap_or(&' '))
                .all(|c| c == '#'))
        .map(|(x, y)| x*y)
        .sum::<isize>();
    println!("Result: {}", result);

    #[cfg(debug_assertions)]
    let mut robot = map.find_robot();

    let mov_main: Vec<&str> = vec!["A", "A",
                                   "B", "C", "B", "C", "B", "C",
                                   "B", "A"];
    let mov_a = vec!["R", "10", "L", "12", "R", "6"];
    let mov_b = vec!["R", "6", "R", "10", "R", "12", "R", "6"];
    let mov_c = vec!["R", "10", "L", "12", "L", "12"];

    #[cfg(debug_assertions)]
    {
        println!("{}", mov_main.join(",").len());
        println!("{}", mov_a.join(",").len());
        println!("{}", mov_b.join(",").len());
        println!("{}", mov_c.join(",").len());
        println!("=======");

        let mut movs = HashMap::new();
        movs.insert("A", &mov_a);
        movs.insert("B", &mov_b);
        movs.insert("C", &mov_c);

        for mov in &mov_main {
            let routine = movs.get(mov).unwrap();
            for command in routine.iter() {
                step(&mut robot, &map, command)?;
            }
        }

        map.map.insert(robot.pos, match robot.dir {
            Direction::North => '^',
            Direction::East => '>',
            Direction::West => '<',
            Direction::South => 'v',
        });
        map.draw();
        map.map.insert(robot.pos, '#');

        let steps = map.explore(&mut robot);
        println!("{} steps left", steps.len());
    }

    program.set(0, 2);
    let mut code = Intcode::new(&program);
    for sub in &[mov_main, mov_a, mov_b, mov_c] {
        for c in sub.join(",").chars() {
            code.input.push(c as i64);
        }
        code.input.push('\n' as i64);
    }
    code.input.push('n' as i64);
    code.input.push('\n' as i64);

    for o in code {
        if o > 255 {
            println!("Result2: {}", o);
        } else {
            #[cfg(debug_assertions)]
            print!("{}", o as u8 as char);
        }
    }

    Ok(())
}
