extern crate anyhow;
extern crate num_enum;

use anyhow::{anyhow, Result};
use num_enum::TryFromPrimitive;
use std::convert::{Into, TryFrom};
use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::os::unix::io::FromRawFd;

type Program = Vec<i32>;

fn input() -> Program {
    io::BufReader::new(match env::args().nth(1) {
        None => unsafe {fs::File::from_raw_fd(0)},
        Some(filename) => fs::File::open(filename).unwrap(),
    }).split(',' as u8)
        .map(|b| b.map_err(Into::into)
             .and_then(|b| String::from_utf8(b).map_err(Into::into))
             .and_then(|s| s.trim().parse().map_err(Into::into)))
        .map(|s: Result<i32>| s.unwrap())
        .collect()
}

#[derive(TryFromPrimitive, PartialEq, Debug)]
#[repr(i32)]
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
    Halt = 99
}

#[derive(TryFromPrimitive, Debug)]
#[repr(i32)]
enum Mode {
    Position,
    Immediate
}

struct Intcode {
    program: Program,
    pc: usize,
    output: Vec<i32>,
    input: Vec<i32>,
    opcode: OpCode,
    current_code: i32,
}

impl Intcode {
    fn new(program: Program, input: Vec<i32>) -> Intcode {
        Intcode {
            program: program,
            pc: 0,
            output: Vec::new(),
            input: input,
            opcode: OpCode::Nop,
            current_code: 0
        }
    }

    fn _next(&mut self) -> i32 {
        let result = self.program[self.pc];
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

    fn _load(&mut self) -> Result<i32> {
        let value = self._next();
        let result = match Mode::try_from(self.current_code % 10)? {
            Mode::Position => self.program[value as usize],
            Mode::Immediate => value,
        };
        Ok(result)
    }

    fn _store(&mut self, value: i32) {
        let a = self._next() as usize;
        self.program[a] = value;
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
                    self.pc = v2 as usize;
                }
            },
            OpCode::JZ => {
                let v1 = self._load()?;
                let v2 = self._load()?;
                if v1 == 0 {
                    self.pc = v2 as usize;
                }
            },
            OpCode::LT => {
                let v1 = self._load()?;
                let v2 = self._load()?;
                self._store((v1 < v2) as i32);
            }
            OpCode::EQ => {
                let v1 = self._load()?;
                let v2 = self._load()?;
                self._store((v1 == v2) as i32);
            },
            OpCode::Halt => {
                self.pc -= 1;
            },
        }
        Ok(())
    }

    fn run(program: &Program, input: Vec<i32>) -> Result<Vec<i32>> {
        let mut code = Intcode::new(program.clone(), input);
        while code.opcode != OpCode::Halt {
            code.step()?;
        }
        Ok(code.output.clone())
    }
}

fn main() -> Result<()> {
    let program = input();

    let mut output1 = Intcode::run(&program, vec![1])?;
    let result1 = output1
        .pop()
        .ok_or(anyhow!("Program 1 no output"))?;
    println!("Result: {}", result1);
    if output1.drain(0..).any(|x| x != 0) {
        panic!("Expected only 0's, got {:?}", output1);
    }
    let result2 = Intcode::run(&program, vec![5])
        .map(|mut o| o.pop())
        .transpose()
        .ok_or(anyhow!("Program 2 no output"))??;
    println!("Result2: {}", result2);
    Ok(())
}
