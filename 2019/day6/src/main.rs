use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::os::unix::io::FromRawFd;


fn input() -> Vec<String> {
    io::BufReader::new(match env::args().nth(1) {
        None => unsafe {fs::File::from_raw_fd(0)},
        Some(filename) => fs::File::open(filename).unwrap(),
    }).lines()
        .map(|l| l.unwrap())
        .collect()
}

fn above(parents: &HashMap<String, String>, object: &str) -> HashSet<String> {
    let mut result = HashSet::new();
    let mut cursor = object.to_string();

    loop {
        match parents.get(&cursor) {
            Some(o) => {
                cursor = o.to_string();
                result.insert(cursor.clone());
            },
            None => break,
        };
    }

    result
}

fn main() {
    let mut orbits = HashMap::new();
    let mut left = HashSet::new();
    let mut right = HashSet::new();
    let mut parents = HashMap::new();

    for l in input() {
        let mut s = l.split(')');
        let o1 = s.next().unwrap();
        let o2 = s.next().unwrap();
        let entry = orbits.entry(o1.to_string()).or_insert(Vec::new());
        parents.insert(o2.to_string(), o1.to_string());
        entry.push(o2.to_string());
        left.insert(o1.to_string());
        right.insert(o2.to_string());
        // println!("{}", l);
    }

    let mut queue = Vec::new();
    for o in left.difference(&right) {
        queue.push((o, 0));
    }

    let mut result = 0;
    while !queue.is_empty() {
        let (item, depth) = queue.remove(0);
        result += depth;
        match orbits.get(item) {
            None => {},
            Some(v) => for i in v {
                queue.push((i, depth+1));
            }
        };
        // println!("Took {} at depth {}", item, depth);
    }
    println!("Result: {}", result);

    let result2 = above(&parents, "YOU")
        .symmetric_difference(&above(&parents, "SAN"))
        .count();

    println!("Result2: {}", result2);
}
