extern crate anyhow;
extern crate getopts;
extern crate num_enum;
extern crate permutohedron;

use anyhow::{anyhow, Result};
use getopts::Options;
use num_enum::TryFromPrimitive;
use permutohedron::LexicalPermutation;
use std::cmp;
use std::convert::{Into, TryFrom};
use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::os::unix::io::FromRawFd;

type Program = Vec<i32>;

fn input(args: Vec<String>) -> Program {
    io::BufReader::new(match args.iter().next() {
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

    fn _run(program: &Program, input: Vec<i32>) -> Result<Vec<i32>> {
        let mut code = Intcode::new(program.clone(), input);
        while code.opcode != OpCode::Halt {
            code.step()?;
        }
        Ok(code.output.clone())
    }
}


fn try_phases(program: &Program, phases: Vec<i32>) ->Result<i32> {
    let mut signal = 0;
    let mut intcodes = Vec::new();
    for i in 0..5 {
        intcodes.push(Intcode::new(program.clone(), vec![phases[i]]));
    }

    loop {
        for i in 0..5 {
            intcodes[i].input.push(signal);
            loop {
                intcodes[i].step()?;
                match intcodes[i].opcode {
                    OpCode::Output => {
                        signal = intcodes[i].output.pop()
                            .ok_or(anyhow!("Program 2 no output"))?;
                        break;
                    },
                    OpCode::Halt => {
                        if i == 4 {
                            return Ok(signal);
                        } else {
                            break;
                        }
                    }
                    _ => {}
                };
            }
        }
    }
}


fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.optflag("1", "", "part 1");
    opts.optflag("2", "", "part 2");
    opts.optflag("h", "help", "print this help");
    let matches = opts.parse(&args[1..])?;

    if matches.opt_present("h") {
        let brief = format!("Usage: {} [options] FILE", args[0]);
        print!("{}", opts.usage(&brief));
        return Ok(());
    }

    let do_part_1 = matches.opt_present("1") || !matches.opt_present("2");
    let do_part_2 = matches.opt_present("2") || !matches.opt_present("1");

    let program = input(matches.free);

    let mut phases = [0,1,2,3,4];
    let mut result = 0;
    if do_part_1 {
        loop {
            let output = try_phases(&program, phases.to_vec())?;
            result = cmp::max(result, output);
            if !phases.next_permutation() {
                break;
            }
        }
        println!("Result: {}", result);
    }

    phases = [5,6,7,8,9];
    result = 0;
    if do_part_2 {
        loop {
            let output = try_phases(&program, phases.to_vec())?;
            result = cmp::max(result, output);
            if !phases.next_permutation() {
                break;
            }
        }
        println!("Result2: {}", result);
    }
    Ok(())
}
