use anyhow::Result;
use num_enum::TryFromPrimitive;
use std::collections::HashMap;
use std::convert::{Into, TryFrom};
use std::fs;
use std::io::{BufRead, BufReader};
use std::iter::FromIterator;
use std::os::unix::io::FromRawFd;

#[derive(Clone)]
pub struct Program(HashMap<u64, i64>);

impl Program {
    pub fn load(maybe_filename: Option<&String>) -> Program {
        let reader = BufReader::new(match maybe_filename {
            None => unsafe {fs::File::from_raw_fd(0)},
            Some(filename) => fs::File::open(filename).unwrap(),
        });
        let iter = reader.split(b',')
            .map(|b| b.map_err(Into::into)
                 .and_then(|b| String::from_utf8(b).map_err(Into::into))
                 .and_then(|s| s.trim().parse().map_err(Into::into)))
            .map(|s: Result<i64>| s.unwrap());
        Program{0: HashMap::from_iter((0 as u64..).zip(iter))}
    }

    pub fn set(&mut self, addr: u64, value: i64) {
        self.0.insert(addr, value);
    }
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

pub struct Intcode {
    program: Program,
    pc: u64,
    output: Vec<i64>,
    pub input: Vec<i64>,
    opcode: OpCode,
    current_code: i64,
    relative_base: i64,
}

impl Intcode {
    pub fn new(program: &Program) -> Intcode {
        Intcode {
            program: program.clone(),
            pc: 0,
            output: Vec::new(),
            input: Vec::new(),
            opcode: OpCode::Nop,
            current_code: 0,
            relative_base: 0,
        }
    }

    fn _get(&self, addr: u64) -> i64 {
        *self.program.0.get(&addr).or(Some(&0)).unwrap()
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
        self.program.0.insert(addr as u64, value);
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
}

impl Iterator for Intcode {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        while self.output.is_empty() {
            if self.opcode == OpCode::Halt {
                return None;
            }
            self.step().expect("step");
        }
        Some(self.output.remove(0))
    }
}

