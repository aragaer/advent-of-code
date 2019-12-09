extern crate anyhow;
extern crate getopts;
extern crate num_enum;

use anyhow::Result;
use getopts::Options;
use num_enum::TryFromPrimitive;
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

    fn run(program: &Program, input: Vec<i64>) -> Result<Vec<i64>> {
        let mut code = Intcode::new(program.clone(), input);
        while code.opcode != OpCode::Halt {
            code.step()?;
        }
        Ok(code.output.clone())
    }
}


fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.optmulti("i", "input", "", "");
    let matches = opts.parse(&args[1..])?;
    let inputs: Vec<i64> = matches.opt_strs("input")
        .into_iter()
        .map(|s| s.parse().unwrap())
        .collect();

    let program = input(matches.free);

    let output = Intcode::run(&program, inputs)?;
    for x in output.iter() {
        println!("{}", x);
    }
    Ok(())
}
