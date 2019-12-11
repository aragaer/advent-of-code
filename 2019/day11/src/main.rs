extern crate anyhow;
extern crate getopts;
extern crate num;
extern crate num_enum;

use anyhow::Result;
use getopts::Options;
use num_enum::TryFromPrimitive;
use num::complex::Complex;
use std::cmp::{min, max};
use std::collections::HashMap;
use std::convert::{Into, TryFrom};
use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::iter::FromIterator;
use std::os::unix::io::FromRawFd;

type Program = HashMap<u64, i64>;

fn input(args: Vec<String>) -> Program {
    let reader = io::BufReader::new(match args.iter().next() {
        None => unsafe {fs::File::from_raw_fd(0)},
        Some(filename) => fs::File::open(filename).unwrap(),
    });
    let iter = reader.split(',' as u8)
        .map(|b| b.map_err(Into::into)
             .and_then(|b| String::from_utf8(b).map_err(Into::into))
             .and_then(|s| s.trim().parse().map_err(Into::into)))
        .map(|s: Result<i64>| s.unwrap());
    HashMap::from_iter((0 as u64..).zip(iter))
}

#[derive(TryFromPrimitive, PartialEq, Debug)]
#[repr(i64)]
enum OpCode {
    Nop,
    Add = 1,
    Mul = 2,
    Input = 3,
    Output = 4,
    JNZ = 5,
    JZ = 6,
    LT = 7,
    EQ = 8,
    REL = 9,
    Halt = 99
}

#[derive(TryFromPrimitive, Debug)]
#[repr(i64)]
enum Mode {
    Position,
    Immediate,
    Relative
}

struct Intcode {
    program: Program,
    pc: u64,
    output: Vec<i64>,
    input: Vec<i64>,
    opcode: OpCode,
    current_code: i64,
    relative_base: i64,
}

impl Intcode {
    fn new(program: Program, input: Vec<i64>) -> Intcode {
        Intcode {
            program: program,
            pc: 0,
            output: Vec::new(),
            input: input,
            opcode: OpCode::Nop,
            current_code: 0,
            relative_base: 0,
        }
    }

    fn _get(&self, addr: u64) -> i64 {
        *self.program.get(&addr).or(Some(&0)).unwrap()
    }

    fn _next(&mut self) -> i64 {
        let result = self._get(self.pc);
        self.current_code /= 10;
        self.pc += 1;
        result
    }

    fn _next_opcode(self: &mut Self) -> Result<&OpCode> {
        // println!("Code {} at pc {}", self.program[self.pc], self.pc);
        self.current_code = self._next();
        self.opcode = OpCode::try_from(self.current_code % 100)?;
        self.current_code /= 10;
        Ok(&self.opcode)
    }

    fn _load(&mut self) -> Result<i64> {
        let value = self._next();
        let result = match Mode::try_from(self.current_code % 10)? {
            Mode::Position => self._get(value as u64),
            Mode::Immediate => value,
            Mode::Relative => {
                let addr = self.relative_base + value;
                self._get(addr as u64)
            },
        };
        Ok(result)
    }

    fn _store(&mut self, value: i64) {
        let a = self._next();
        let addr = match Mode::try_from(self.current_code % 10).unwrap() {
            Mode::Position => a,
            Mode::Immediate => panic!("store to immediate position"),
            Mode::Relative => a + self.relative_base,
        };
        self.program.insert(addr as u64, value);
    }

    fn step(&mut self) -> Result<()> {
        match self._next_opcode()? {
            OpCode::Nop => {
            },
            OpCode::Add => {
                let v1 = self._load()?;
                let v2 = self._load()?;
                self._store(v1+v2);
            },
            OpCode::Mul => {
                let v1 = self._load()?;
                let v2 = self._load()?;
                self._store(v1*v2);
            },
            OpCode::Input => {
                let v1 = self.input.remove(0);
                self._store(v1);
            },
            OpCode::Output => {
                let v1 = self._load()?;
                self.output.push(v1);
            },
            OpCode::JNZ => {
                let v1 = self._load()?;
                let v2 = self._load()?;
                if v1 != 0 {
                    // println!("Jump to {}", v2);
                    self.pc = v2 as u64;
                }
            },
            OpCode::JZ => {
                let v1 = self._load()?;
                let v2 = self._load()?;
                if v1 == 0 {
                    self.pc = v2 as u64;
                }
            },
            OpCode::LT => {
                let v1 = self._load()?;
                let v2 = self._load()?;
                self._store((v1 < v2) as i64);
            }
            OpCode::EQ => {
                let v1 = self._load()?;
                let v2 = self._load()?;
                self._store((v1 == v2) as i64);
            },
            OpCode::REL => {
                self.relative_base += self._load()?;
            },
            OpCode::Halt => {
                self.pc -= 1;
            },
        }
        Ok(())
    }

    fn _run(program: &Program, input: Vec<i64>) -> Result<Vec<i64>> {
        let mut code = Intcode::new(program.clone(), input);
        while code.opcode != OpCode::Halt {
            code.step()?;
        }
        Ok(code.output.clone())
    }
}


fn paint(program: &Program, picture: &mut HashMap<Complex<i32>, i64>) -> Result<()> {
    let mut position: Complex<i32> = Complex::new(0, 0);
    let mut direction: Complex<i32> = Complex::i();

    let mut robot = Intcode::new(program.clone(), Vec::new());
    loop {
        robot.input.push(*picture.get(&position.clone()).unwrap_or(&0));
        loop {
            robot.step()?;
            match robot.opcode {
                OpCode::Input => break,
                OpCode::Halt => return Ok(()),
                _ => continue,
            }
        }
        while robot.opcode != OpCode::Output {
            robot.step()?;
        }
        picture.insert(position.clone(), robot.output.remove(0));
        robot.step()?;
        while robot.opcode != OpCode::Output {
            robot.step()?;
        }
        direction *= match robot.output.remove(0) {
            0 => Complex::i(),
            1 => -Complex::i(),
            x => panic!("Turn {}", x),
        };
        position += direction;
    }
}


fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let opts = Options::new();
    let matches = opts.parse(&args[1..])?;

    let program = input(matches.free);

    let mut picture = HashMap::new();
    paint(&program, &mut picture)?;
    println!("Result: {}", picture.len());

    let mut picture = HashMap::new();
    picture.insert(Complex::new(0, 0), 1);
    paint(&program, &mut picture)?;
    let (mut left, mut right, mut top, mut bottom) = (0, 0, 0, 0);
    for k in picture.keys() {
        left = min(left, k.re);
        right = max(right, k.re);
        top = min(top, k.im);
        bottom = max(bottom, k.im+1);
    }

    for j in (top..bottom).rev() {
        let s: String = (left..right).map(|i| {
            let point = Complex::new(i as i32, j as i32);
            match picture.get(&point) {
                None    => ' ',
                Some(0) => ' ',
                Some(1) => '#',
                Some(x) => panic!("unknown color {}", x),
            }
        }).collect();
        println!("{}", s);
    }

    Ok(())
}
