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

fn step(program: &mut Vec<i32>, position: usize) -> bool {
    let len = program.len();
    let a1 = program[(position + 1) % len] as usize % len;
    let a2 = program[(position + 2) % len] as usize % len;
    let a3 = program[(position + 3) % len] as usize % len;
    let v1 = program[a1];
    let v2 = program[a2];
    match program[position] {
        1 => {
            program[a3] = v1 + v2;
            true
        },
        2 => {
            program[a3] = v1 * v2;
            true
        },
        99 => false,
        _ => panic!("wat!"),
    }
}

fn calculate(orig_program: &Vec<i32>, inp1: i32, inp2: i32) -> i32 {
    let mut program = orig_program.clone();
    program[1] = inp1;
    program[2] = inp2;

    let mut position = 0;

    while step(&mut program, position) {
        position += 4;
        position %= program.len();
    }

    return program[0];
}

fn part1(orig_program: &Vec<i32>) -> i32 {
    calculate(orig_program, 12, 2)
}

fn part2(orig_program: &Vec<i32>) -> i32 {
    let mut result = 0;
    while calculate(orig_program, result / 100, result % 100) != 19690720 {
        result += 1;
    }
    result
}

fn main() {
    let orig_program: Vec<i32> = input()
        .map(|o| o.unwrap().parse().unwrap())
        .collect();

    println!("Result: {}", part1(&orig_program));
    println!("Result: {}", part2(&orig_program));
}
