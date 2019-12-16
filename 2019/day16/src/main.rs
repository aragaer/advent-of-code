use anyhow::Result;
use getopts::Options;
use std::cmp::min;
use std::env;
use std::fs;
use std::io::{Read, BufReader};
use std::os::unix::io::FromRawFd;

fn input(args: Vec<String>) -> impl Iterator<Item=i32> {
    BufReader::new(match args.iter().next() {
        None => unsafe {fs::File::from_raw_fd(0)},
        Some(filename) => fs::File::open(filename).unwrap(),
    }).bytes()
        .map(|b| b.unwrap())
        .filter(|&b| b >= '0' as u8 && b <= '9' as u8)
        .map(|b| b as i32 - '0' as i32)
}

fn last_digit(n: i32) -> i32 {
    n.abs() % 10
}

fn fft(input: Vec<i32>) -> Vec<i32> {
    let len = input.len();
    let mut result = Vec::with_capacity(len);
    for n in 1..=len {
        let mut r: i32 = 0;
        let mut k = n-1;
        while k < len {
            r += input[k..min(n+k, len)].iter().sum::<i32>();
            k += n * 2;
            if k >= len {
                break;
            }
            r -= input[k..min(n+k, len)].iter().sum::<i32>();
            k += n * 2;
        }
        result.push(last_digit(r));
    }
    result
}

fn fft_tail(input: Vec<i32>) -> Vec<i32> {
    let len = input.len();
    let mut result = Vec::with_capacity(len);
    let mut ps = input.iter().sum::<i32>();
    for n in 0..len {
        result.push(last_digit(ps));
        ps -= input[n];
    }
    result
}

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();
    let opts = Options::new();
    let matches = opts.parse(&args[1..])?;

    let input: Vec<_> = input(matches.free).collect();
    let mut digits = input.clone();

    for _ in 0..100 {
        digits = fft(digits);
    }
    let result: String = (0..8).map(|n| digits[n].to_string()).collect();
    println!("Result: {}", result);

    let len = input.len();
    let mut digits2: Vec<i32> = input.iter().cycle()
        .take(len * 10000)
        .map(|&d| d)
        .collect();
    let offset_s: String = (0..7).map(|n| digits2[n].to_string()).collect();
    let offset: usize = offset_s.parse().unwrap();
    for _ in 0..100 {
        digits2 = fft_tail(digits2);
    }
    let result2: String = (0..8).map(|n| digits2[n+offset].to_string()).collect();
    println!("Result2: {}", result2);

    Ok(())
}
