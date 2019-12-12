use getopts::Options;
use itertools::Itertools;
use num::integer::lcm;

type P1d = i32;

#[derive(Clone, Copy, Debug, Eq)]
struct Moon1 {
    p: P1d,
    v: P1d,
    a: P1d
}

impl Moon1 {
    fn new(x: i32) -> Moon1 {
        Moon1 {
            p: x,
            v: 0,
            a: 0,
        }
    }

    fn r#move(&mut self) {
        self.v += self.a;
        self.p += self.v;
        self.a = 0;
    }

    fn influence(&mut self, other: &mut Self) {
        let d = other.p - self.p;
        self.a += d.signum();
        other.a -= d.signum();
    }
}

impl PartialEq for Moon1 {
    fn eq(&self, other: &Self) -> bool {
        self.p == other.p && self.v == other.v
    }
}

#[derive(Clone, Debug)]
struct Moon {
    x: Moon1,
    y: Moon1,
    z: Moon1,
}


impl Moon {
    fn new(x: i32, y: i32, z: i32) -> Moon {
        Moon {
            x: Moon1::new(x),
            y: Moon1::new(y),
            z: Moon1::new(z),
        }
    }

    fn r#move(&mut self) {
        self.x.r#move();
        self.y.r#move();
        self.z.r#move();
    }

    fn influence(&mut self, other: &mut Self) {
        self.x.influence(&mut other.x);
        self.y.influence(&mut other.y);
        self.z.influence(&mut other.z);
    }

    fn energy(&self) -> i32 {
        (self.x.p.abs() + self.y.p.abs() + self.z.p.abs())
            * (self.x.v.abs() + self.y.v.abs() + self.z.v.abs())
    }
}

fn energy_after(moons: &mut [Moon], steps: i32) -> i32 {
    for _ in 0..steps {
        for idxs in (0..4).combinations(2) {
            let (lefts, rights) = moons.split_at_mut(idxs[1]);
            lefts[idxs[0]].influence(&mut rights[0]);
        }
        for moon in moons.iter_mut() {
            moon.r#move();
        }
    }
    moons.iter().map(|m| m.energy().clone()).sum::<i32>()
}

fn state2key(moons: &Vec<Moon1>) -> (i32, i32, i32, i32, i32, i32, i32, i32) {
    (moons[0].p, moons[0].v,
     moons[1].p, moons[1].v,
     moons[2].p, moons[2].v,
     moons[3].p, moons[3].v)
}

fn find_loop1(mut moons: Vec<Moon1>) -> u64 {
    let initial = state2key(&moons);
    let mut step = 0;
    loop {
        step += 1;
        for idxs in (0..4).combinations(2) {
            let (lefts, rights) = moons.split_at_mut(idxs[1]);
            lefts[idxs[0]].influence(&mut rights[0]);
        }
        for moon in moons.iter_mut() {
            moon.r#move();
        }
        if state2key(&moons) == initial {
            return step;
        }
    };
}

fn find_loop(moons: &mut [Moon]) -> u64 {
    let x = find_loop1(moons.iter().map(|m| m.x).collect());
    let y = find_loop1(moons.iter().map(|m| m.y).collect());
    let z = find_loop1(moons.iter().map(|m| m.z).collect());
    lcm(x, lcm(y, z))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut opts = Options::new();
    opts.optopt("e", "example", "", "");
    let matches = opts.parse(&args[1..]).expect("parse args");

    let (moons, iterations) = if matches.opt_present("e") {
        match matches.opt_get("e").expect("example") {
            Some(1) => ([Moon::new(-1, 0, 2),
                         Moon::new(2, -10, -7),
                         Moon::new(4, -8, 8),
                         Moon::new(3, 5, -1)], 10),
            Some(2) => ([Moon::new(-8, -10, 0),
                         Moon::new(5, 5, 10),
                         Moon::new(2, -7, 3),
                         Moon::new(9, -8, -3)], 100),
            Some(x) => panic!("Example should be 1 or 2, not {}", x),
            None => panic!("Example should be 1 or 2"),
        }
    } else {
        ([Moon::new(-10, -13, 7),
          Moon::new(1, 2, 1),
          Moon::new(-15, -3, 13),
          Moon::new(3, 7, -4)], 1000)
    };

    println!("Result: {}", energy_after(&mut moons.clone(), iterations));
    println!("Result2: {}", find_loop(&mut moons.clone()));
}
