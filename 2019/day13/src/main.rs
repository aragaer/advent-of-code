use anyhow::Result;
use getopts::Options;
use std::default::Default;
use std::env;

mod intcode;

use crate::intcode::{Intcode, Program};

struct Arcade {
    code: Intcode,
    screen: Vec<Vec<char>>,
    score: i32,
    ball: usize,
    paddle: usize,
}

impl Arcade {
    fn new(program: &Program) -> Arcade {
        Arcade {
            code: Intcode::new(program, vec![]),
            screen: Vec::new(),
            score: 0,
            ball: 0,
            paddle: 0,
        }
    }

    fn set_stick(&mut self) {
        self.code.input.clear();
        self.code.input.push((self.ball as i64 - self.paddle as i64).signum());
    }

    fn run_to_halt(&mut self) -> Result<()> {
        loop {
            self.set_stick();
            let x = self.code.next();
            let y = self.code.next();
            let o = self.code.next();
            let (x, y, o) = match (x, y, o) {
                (None, None, None) => {
                    return Ok(());
                },
                (Some(x), Some(y), Some(z)) => (x, y, z),
                _ => panic!("need exactly 3 items"),
            };
            if (x, y) == (-1, 0) {
                self.score = o as i32;
            } else {
                let x = x as usize;
                let y = y as usize;
                if y >= self.screen.len() {
                    self.screen.resize_with(y+1, Vec::new);
                }
                let line = &mut self.screen[y];
                if x >= line.len() {
                    line.resize_with(x+1, Default::default);
                }
                line[x] = match o {
                    0 => '.',
                    1 => '#',
                    2 => 'B',
                    3 => {
                        self.paddle = x;
                        '='
                    },
                    4 => {
                        self.ball = x;
                        '@'
                    },
                    x => panic!("Unknown block {}", x),
                }
            }
        }
    }

    fn _draw(&self) {
        println!("Score: {}", self.score);
        for line in self.screen.iter() {
            let s: String = line.iter().collect();
            println!("{}", s);
        }
    }
}


fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let opts = Options::new();
    let matches = opts.parse(&args[1..])?;

    let mut program = Program::load(matches.free);

    let mut arcade = Arcade::new(&program);
    arcade.run_to_halt()?;
    let result: usize = arcade.screen.iter()
        .map(|l| l.iter().filter(|c| **c == 'B').count())
        .sum();
    println!("Result: {}", result);

    program.set(0, 2);
    let mut arcade = Arcade::new(&program);
    arcade.run_to_halt()?;
    println!("Result2: {}", arcade.score);

    Ok(())
}
