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

enum OpCode {
    Add,
    Mul,
    Input,
    Output,
    JNZ,
    JZ,
    LT,
    EQ,
    Halt
}

fn op(opcode: i32) -> OpCode {
    match opcode {
        1 => OpCode::Add,
        2 => OpCode::Mul,
        3 => OpCode::Input,
        4 => OpCode::Output,
        5 => OpCode::JNZ,
        6 => OpCode::JZ,
        7 => OpCode::LT,
        8 => OpCode::EQ,
        99 => OpCode::Halt,
        x => panic!("Unknown opcode: {}", x),
    }
}

enum Mode {
    Position,
    Immediate
}

struct Instruction {
    code: i32,
    opcode: OpCode,
}

impl Instruction {
    fn new(instruction: i32) -> Instruction {
        Instruction {
            code: instruction,
            opcode: op(instruction % 100),
        }
    }

    fn mode(self: &Self, n: usize) -> Mode {
        let mut code = self.code / 100;
        for _ in 0..n {
            code /= 10
        }
        match code % 10 {
            0 => Mode::Position,
            1 => Mode::Immediate,
            x => panic!("Unknown mode {}", x),
        }
    }
}

struct State {
    program: Vec<i32>,
    pc: usize,
    output: Option<i32>,
    input: i32,
}

impl State {
    fn next(self: &Self) -> Instruction {
        // println!("Code {} at pc {}", self.program[self.pc], self.pc);
        Instruction::new(self.program[self.pc])
    }

    fn advance(self: &mut Self, count: usize) {
        self.pc += count;
    }

    fn value_at(self: &Self, pos: usize, mode: Mode) -> i32 {
        let len = self.program.len();
        match mode {
            Mode::Position => self.program[self.program[pos] as usize % len],
            Mode::Immediate => self.program[pos],
        }
    }
}


fn step(state: &mut State) -> bool {
    let instruction = state.next();
    let pos = state.pc;
    match instruction.opcode {
        OpCode::Add => {
            let v1 = state.value_at(pos+1, instruction.mode(0));
            let v2 = state.value_at(pos+2, instruction.mode(1));
            let a3 = state.program[pos+3] as usize;
            state.program[a3] = v1 + v2;
            state.advance(4);
            true
        },
        OpCode::Mul => {
            let v1 = state.value_at(pos+1, instruction.mode(0));
            let v2 = state.value_at(pos+2, instruction.mode(1));
            let a3 = state.program[pos+3] as usize;
            state.program[a3] = v1 * v2;
            state.advance(4);
            true
        },
        OpCode::Input => {
            let a1 = state.program[pos+1] as usize;
            state.program[a1] = state.input;
            // println!("Store {} at {}", state.input, a1);
            state.advance(2);
            true
        },
        OpCode::Output => {
            let v1 = state.value_at(pos+1, instruction.mode(0));
            state.output = Some(v1);
            state.advance(2);
            true
        },
        OpCode::JNZ => {
            let v1 = state.value_at(pos+1, instruction.mode(0));
            let v2 = state.value_at(pos+2, instruction.mode(1));
            if v1 == 0 {
                state.advance(3);
            } else {
                // println!("Jump to {}", v2);
                state.pc = v2 as usize;
            }
            true
        },
        OpCode::JZ => {
            let v1 = state.value_at(pos+1, instruction.mode(0));
            let v2 = state.value_at(pos+2, instruction.mode(1));
            if v1 != 0 {
                state.advance(3);
            } else {
                state.pc = v2 as usize;
            }
            true
        },
        OpCode::LT => {
            let v1 = state.value_at(pos+1, instruction.mode(0));
            let v2 = state.value_at(pos+2, instruction.mode(1));
            let a3 = state.program[pos+3] as usize;
            if v1 < v2 {
                state.program[a3] = 1;
            } else {
                state.program[a3] = 0;
            }
            state.advance(4);
            true
        },
        OpCode::EQ => {
            let v1 = state.value_at(pos+1, instruction.mode(0));
            let v2 = state.value_at(pos+2, instruction.mode(1));
            let a3 = state.program[pos+3] as usize;
            // println!("Compare {} and {}", v1, v2);
            if v1 == v2 {
                state.program[a3] = 1;
            } else {
                state.program[a3] = 0;
            }
            state.advance(4);
            true
        },
        OpCode::Halt => {
            state.advance(1);
            false
        },
    }
}

fn run(orig_program: &Vec<i32>, arg: i32) {
    let mut state = State {
        program: orig_program.clone(),
        pc: 0,
        output: None,
        input: arg,
    };

    while step(&mut state) {
        match state.output {
            Some(x) => println!("Output: {}", x),
            None => {},
        }
        state.output = None;
    }
}

fn main() {
    let orig_program: Vec<i32> = input()
        .map(|o| o.unwrap().trim().parse().unwrap())
        .collect();

    println!("Input 1:");
    run(&orig_program, 1);
    println!("Input 5:");
    run(&orig_program, 5);
}
