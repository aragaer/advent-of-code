extern crate anyhow;
extern crate getopts;
extern crate itertools;
extern crate ndarray;

use anyhow::{anyhow, Result};
use getopts::Options;
use itertools::Itertools;
use ndarray::{Array, Axis};
use std::env;
use std::fs;
use std::io::Read;
use std::os::unix::io::FromRawFd;

fn input(args: &Vec<String>) -> impl Iterator<Item=i8> {
    match args.iter().next() {
        None => unsafe {fs::File::from_raw_fd(0)},
        Some(filename) => fs::File::open(filename).unwrap(),
    }.bytes()
        .map(|c| c.unwrap() as i8 - '0' as i8)
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.reqopt("s", "size", "", "");
    let matches = opts.parse(&args[1..])?;

    let image: Vec<i8> = input(&matches.free).collect();
    let (width, height) = matches.opt_str("s")
        .ok_or(anyhow!("size"))?
        .split('x')
        .map(|s| s.parse::<usize>().unwrap())
        .collect_tuple()
        .ok_or(anyhow!("tuple"))?;
    let layers = image.len() / width / height;
    let arr = Array::from_shape_vec((layers, height, width), image)?;

    let result = arr.axis_iter(Axis(0))
        .map(|row| row.iter()
             .fold((0, 0, 0),
                   |(z, o, t), v| match v {
                       0 => (z+1, o, t),
                       1 => (z, o+1, t),
                       2 => (z, o, t+1),
                       _ => (z, o ,t),
                   }))
        .min_by_key(|(z, _, _)| z.clone())
        .map(|(_, o, t)| o*t)
        .ok_or(anyhow!("empty"))?;
    println!("Result: {}", result);

    for cut in arr.axis_iter(Axis(1)) {
        let line: String = cut.axis_iter(Axis(1))
            .map(|column| column.iter()
                 .find_map(|&x| match x {
                     2 => None,
                     1 => Some('#'),
                     0 => Some(' '),
                     _ => Some('?'),
                 })
                 .unwrap_or('?'))
            .collect();
        println!("{}", line);
    }

    Ok(())
}
