extern crate num_enum;

use num_enum::TryFromPrimitive;
use std::convert::TryFrom;
use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::os::unix::io::FromRawFd;

fn input() -> impl Iterator<Item=io::Result<String>> {
    let reader = io::BufReader::new(match env::args().nth(1) {
        None => unsafe {fs::File::from_raw_fd(0)},
        Some(filename) => fs::File::open(filename).unwrap(),
    });
    reader.split(',' as u8)
        .map(|b| b.unwrap())
        .map(String::from_utf8)
        .map(|s| Ok(s.unwrap()))
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
    program: Vec<i32>,
    pc: usize,
    output: Vec<i32>,
    input: Vec<i32>,
    opcode: OpCode,
    current_code: i32,
}

impl Iterator for Intcode {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.program[self.pc];
        self.current_code /= 10;
        self.pc += 1;
        Some(result)
    }
}

impl Intcode {
    fn new(program: &Vec<i32>, input: Vec<i32>) -> Intcode {
        Intcode {
            program: program.clone(),
            pc: 0,
            output: Vec::new(),
            input: input,
            opcode: OpCode::Nop,
            current_code: 0
        }
    }

    fn _next_opcode(self: &mut Self) -> &OpCode {
        // println!("Code {} at pc {}", self.program[self.pc], self.pc);
        self.current_code = self.next().unwrap();
        self.opcode = OpCode::try_from(self.current_code % 100).unwrap();
        self.current_code /= 10;
        &self.opcode
    }

    fn _load(&mut self) -> i32 {
        let value = self.next().unwrap();
        let result = match Mode::try_from(self.current_code % 10).unwrap() {
            Mode::Position => self.program[value as usize],
            Mode::Immediate => value,
        };
        result
    }

    fn _store(&mut self, value: i32) {
        let a = self.next().unwrap() as usize;
        self.program[a] = value;
    }

    fn step(&mut self) {
        match self._next_opcode() {
            OpCode::Nop => {
            },
            OpCode::Add => {
                let v1 = self._load();
                let v2 = self._load();
                self._store(v1+v2);
            },
            OpCode::Mul => {
                let v1 = self._load();
                let v2 = self._load();
                self._store(v1*v2);
            },
            OpCode::Input => {
                let v1 = self.input.remove(0);
                self._store(v1);
            },
            OpCode::Output => {
                let v1 = self._load();
                self.output.push(v1);
            },
            OpCode::JNZ => {
                let v1 = self._load();
                let v2 = self._load();
                if v1 != 0 {
                    // println!("Jump to {}", v2);
                    self.pc = v2 as usize;
                }
            },
            OpCode::JZ => {
                let v1 = self._load();
                let v2 = self._load();
                if v1 == 0 {
                    self.pc = v2 as usize;
                }
            },
            OpCode::LT => {
                let v1 = self._load();
                let v2 = self._load();
                self._store((v1 < v2) as i32);
            }
            OpCode::EQ => {
                let v1 = self._load();
                let v2 = self._load();
                self._store((v1 == v2) as i32);
            },
            OpCode::Halt => {
                self.pc -= 1;
            },
        }
    }

    fn run(program: &Vec<i32>, input: Vec<i32>) -> Vec<i32> {
        let mut code = Intcode::new(program, input);
        while code.opcode != OpCode::Halt {
            code.step();
        }
        code.output.clone()
    }
}

fn main() {
    let program: Vec<i32> = input()
        .map(|o| o.unwrap().trim().parse().unwrap())
        .collect();

    let mut output1 = Intcode::run(&program, vec![1]);
    let result1 = output1.pop().unwrap();
    println!("Result: {}", result1);
    if output1.iter().any(|x| *x != 0) {
        panic!("Expected only 0's, got {:?}", output1);
    }
    let result2 = Intcode::run(&program, vec![5]).pop().unwrap();
    println!("Result2: {}", result2);
}
