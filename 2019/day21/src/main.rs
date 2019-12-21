use std::env;

mod intcode;

use crate::intcode::{Intcode, Program};

fn run(program: &Program, instructions: Vec<&str>) -> i64 {
    let mut code = Intcode::new(&program);
    loop {
        let o = code.next().unwrap();
        if o > 255 {
            println!("Result: {}", o);
        } else {
            #[cfg(debug_assertions)]
            print!("{}", o as u8 as char);
            if o as u8 == b'\n' {
                break;
            }
        }
    }

    for line in instructions {
        for c in line.chars() {
            code.input.push(c as i64);
        }
        code.input.push('\n' as i64);
    }

    for o in code {
        if o > 255 {
            return o;
        } else {
            #[cfg(debug_assertions)]
            print!("{}", o as u8 as char);
        }
    }
    panic!("Did not finish");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let program = Program::load(args.get(1));

    let instructions = vec![
        "OR A J",
        "AND C J",
        "NOT J J",
        "AND D J",
        "WALK",
    ];
    let instructions2 = vec![
        "OR A J",
        "AND B J",
        "AND C J",
        "NOT J J",
        "AND D J",
        "OR E T",
        "OR H T",
        "AND T J",
        "RUN",
    ];

    println!("Result: {}", run(&program, instructions));
    println!("Result2: {}", run(&program, instructions2));
}
