use anyhow::{anyhow, Error, Result};
use getopts::Options;
use itertools::Itertools;
use num_integer::div_ceil;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{BufRead, BufReader};
use std::str::FromStr;


fn load(filename: &str) -> impl Iterator<Item=String> {
    BufReader::new(fs::File::open(filename).unwrap())
        .lines()
        .map(|l| l.unwrap())
}

#[derive(Debug)]
struct Chemical(usize, String);

impl FromStr for Chemical {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.split_ascii_whitespace()
            .collect_tuple()
            .map(|(a, c)| Chemical{0: a.parse().unwrap(),
                                   1: c.to_string()})
            .ok_or(anyhow!("Failed to get tuple"))
    }
}

#[derive(Debug)]
struct Reaction {
    output: Chemical,
    inputs: Vec<Chemical>,
}

impl FromStr for Reaction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (i, o) = s.split("=>")
            .collect_tuple()
            .ok_or(anyhow!("No arrow in reaction"))?;
        let o = o.trim().parse::<Chemical>()?;
        let i = i.split(',')
            .map(|i| i.trim().parse::<Chemical>().unwrap())
            .collect();
        Ok(Reaction {
            output: o,
            inputs: i,
        })
    }
}


fn need_ore(reactions: &HashMap<String, Reaction>, count: usize) -> usize {
    let mut leftovers: HashMap<String, usize> = HashMap::new();
    let mut requirements = vec![Chemical{0: count, 1: "FUEL".to_string()}];
    let mut result = 0;

    while !requirements.is_empty() {
        let chemical = requirements.remove(0);
        if chemical.1 == "ORE" {
            panic!("Should not have ORE here");
        }
        let leftover = *leftovers.get(&chemical.1).unwrap_or(&0);
        if  leftover >= chemical.0 {
            //println!("Got {} {} in leftovers", leftover, chemical.1);
            leftovers.insert(chemical.1, leftover - chemical.0);
            continue;
        }
        let need_more = chemical.0 - leftover;
        /*
        println!("Got {} of {} {}, need {} more",
                 leftover, chemical.0, chemical.1, need_more);
        */
        let reaction = reactions.get(&chemical.1).unwrap();
        let count = div_ceil(need_more, reaction.output.0);
        let new_leftover = reaction.output.0*count - need_more;
        /*
        if new_leftover == 0 {
            println!("Reaction will make exactly {} units of {}",
                     chemical.0, chemical.1);
        } else {
            println!("Reaction will make {} units of {}, {} will be left over",
                     reaction.output.0, chemical.1, leftover);
        }
        */
        leftovers.insert(chemical.1, new_leftover);
        for i in reaction.inputs.iter() {
            if i.1 == "ORE" {
                result += i.0 * count;
            } else {
                requirements.push(Chemical{0: i.0*count, 1: i.1.clone()});
            }
        }
    }

    result
}


fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();
    let opts = Options::new();
    let matches = opts.parse(&args[1..])?;

    let mut reactions = HashMap::new();

    for line in load(&matches.free[0]) {
        let r = line.trim().parse::<Reaction>()?;
        reactions.insert(r.output.1.clone(), r);
    }

    println!("Result: {}", need_ore(&reactions, 1));

    let stored = 1000000000000;
    let mut right = 1;
    while need_ore(&reactions, right) <= stored {
        right *= 2;
    }
    let mut left = right / 2;

    while right - left > 1 {
        let mid = (left + right) / 2;
        if need_ore(&reactions, mid) <= stored {
            left = mid;
        } else {
            right = mid;
        }
    }
    println!("Result2: {}", left);

    Ok(())
}
