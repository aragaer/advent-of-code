use std::collections::HashSet;
use std::env;
use std::iter::successors;

mod intcode;

use crate::intcode::{Intcode, Program};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    fn right(self) -> Direction {
        match self {
            Direction::West => Direction::North,
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
        }
    }

    fn left(self) -> Direction {
        match self {
            Direction::West => Direction::South,
            Direction::South => Direction::East,
            Direction::East => Direction::North,
            Direction::North => Direction::West,
        }
    }
}

struct Maze(Intcode);

const BAD_ITEMS: [&str; 5] = [
    "molten lava",
    "infinite loop",
    "escape pod",
    "giant electromagnet",
    "photons",
];

impl Maze {
    fn read_output(&mut self) -> Vec<String> {
        let mut lines: Vec<String> = Vec::new();
        let mut buf: Vec<char> = Vec::new();
        loop {
            buf.clear();
            loop {
                let output = self.0.next();
                if output == None {
                    return lines;
                }
                let ch = output.unwrap() as u8 as char;
                if ch == '\n' {
                    break;
                } else {
                    buf.push(ch);
                }
            }
            let line = buf.iter().collect::<String>();
            if line == "Command?" {
                break;
            }
            lines.push(line);
        }
        lines
    }

    fn read_room(&mut self) -> (String, HashSet<Direction>, HashSet<String>) {
        let lines = self.read_output();
        let name = lines[3].clone();
        let mut doors = HashSet::new();
        let mut items = HashSet::new();
        let mut list_of_doors = false;
        let mut list_of_items = false;
        for l in lines.iter() {
            match l.as_ref() {
                "Doors here lead:" => {
                    list_of_doors = true;
                    continue;
                },
                "Items here:" => {
                    list_of_items = true;
                    continue;
                },
                "" => {
                    list_of_doors = false;
                    list_of_items = false;
                },
                _ => {}
            }
            if list_of_doors {
                doors.insert(match l.as_ref() {
                    "- north" => Direction::North,
                    "- south" => Direction::South,
                    "- east" => Direction::East,
                    "- west" => Direction::West,
                    x => panic!("wat: {}", x),
                });
            }
            if list_of_items {
                items.insert(l[2..].to_string());
            }
        }
        (name, doors, items)
    }

    fn write_command(&mut self, command: &str) {
        for ch in command.chars() {
            self.0.input.push(ch as i64);
        }
        self.0.input.push('\n' as i64);
    }

    fn go(&mut self, dir: Direction) {
        let command = match dir {
            Direction::North => "north",
            Direction::South => "south",
            Direction::East => "east",
            Direction::West => "west",
        };
        self.write_command(command);
    }

    fn take_item(&mut self, item: &str) {
        self.write_command(&("take ".to_string() + item));
        self.read_output();
    }

    fn drop_item(&mut self, item: &str) {
        self.write_command(&("drop ".to_string() + item));
        self.read_output();
    }

    fn bring_items_to_checkpoint(&mut self) -> (Vec<String>, Direction) {
        let mut bad_items = HashSet::new();
        for bad in BAD_ITEMS.iter() {
            bad_items.insert(bad.to_string());
        }
        let mut inventory = Vec::new();
        /* It just happens that if I start with looking Eash
         * I will go North first and explore everything
         * before ending up at Security Checkpoint */
        let mut direction = Direction::East;
        loop {
            let (name, doors, items) = self.read_room();
            for item in items.difference(&bad_items) {
                self.take_item(&item);
                inventory.push(item.to_string());
            }
            direction = successors(Some(direction.left()),
                                   |&d| Some(d.right()))
                .find(|d| doors.contains(d))
                .unwrap();
            if name == "== Security Checkpoint ==" {
                break;
            }
            self.go(direction);
        }
        for item in inventory.iter() {
            self.drop_item(&item);
        }
        (inventory, direction)
    }

    fn try_combo(&mut self, all_items: &[String], combo: i32, to_exit: Direction) -> Option<String> {
        let l = all_items.len();
        let items_to_take: Vec<_> = successors(Some(1), |n| Some(n * 2))
            .take(l)
            .enumerate()
            .filter_map(|(i, b)| if (b & combo) == 0 { None } else { Some(i) })
            .collect();
        for &i in items_to_take.iter() {
            self.take_item(&all_items[i]);
        }
        self.go(to_exit);
        let lines = self.read_output();
        if &lines[11] != "" {
            return Some(lines[11].clone());
        }
        for &i in items_to_take.iter() {
            self.drop_item(&all_items[i]);
        }
        None
    }

    fn guess_weight(&mut self, all_items: &[String], to_exit: Direction) -> String {
        for combo in 0..2_i32.pow(all_items.len() as u32) {
            if let Some(res) = self.try_combo(&all_items, combo, to_exit) {
                return res;
            }
        }
        panic!("weight not found");
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = Program::load(args.get(1));
    let mut maze = Maze{0:Intcode::new(&program)};
    let (all_items, to_exit) = maze.bring_items_to_checkpoint();
    println!("{}", maze.guess_weight(&all_items, to_exit));
}
