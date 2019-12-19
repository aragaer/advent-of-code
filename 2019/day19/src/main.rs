use anyhow::Result;
#[cfg(debug_assertions)]
use itertools::Itertools;
use std::cmp::max;
use std::env;

mod intcode;

use crate::intcode::{Intcode, Program};

fn check(program: &Program, x: i64, y: i64) -> i64 {
    let mut code = Intcode::new(program);
    code.input.push(x);
    code.input.push(y);
    code.next().unwrap()
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let program = Program::load(Some(&args[1]));

    let mut result: i64 = 0;
    let size = 100;

    let mut l = 0;
    let mut r = 1;
    for y in 0..50 {
        l += 1 - check(&program, l, y);
        r += check(&program, r, y);
        r = max(r, l);
        result += r - l;
    }

    println!("Result: {}", result);

    let mut l = (50..(49 + size))
        .fold(l, |l, y| l + 1 - check(&program, l, y));

    let mut y = 50;

    while r - l < size {
        l += 1 - check(&program, l, y + size);
        y += 1;
        r += check(&program, r, y);
        #[cfg(debug_assertions)]
        println!("{}:{}-{}", y, l, r);
    }
    let result2 = l * 10000 + y;
    println!("Result2: {}", result2);

    #[cfg(debug_assertions)]
    for ty in (-2)..(size + 2) {
        let s: String = ((l - 2)..(r + 2))
            .map(|x|
                if check(&program, x, y+ty) == 0 {
                   '.'
                } else if (0..size).contains(&ty) && (l..r).contains(&x) {
                   'O'
                } else {
                   '#'
                })
            .collect();
        println!("{} {}", s, ty+y);
    }

    Ok(())
}
