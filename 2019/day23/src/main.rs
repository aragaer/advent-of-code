use std::collections::HashMap;
use std::env;

mod intcode;

use crate::intcode::{Intcode, Program};

struct NIC {
    address: i64,
    code: Intcode,
    idle: bool,
}

impl NIC {
    fn new(program: &Program, address: i64) -> NIC {
        let mut result = NIC {
            address,
            code: Intcode::new(&program),
            idle: false,
        };
        result.code.input.push(address);
        result
    }
}

#[derive(Clone, Copy)]
struct Message {
    x: i64,
    y: i64,
}

fn step(nics: &mut Vec<NIC>, messages: HashMap<i64, Vec<Message>>) -> HashMap<i64, Vec<Message>> {
    let mut result = HashMap::new();
    for nic in nics.iter_mut() {
        if let Some(q) = messages.get(&nic.address) {
            if nic.idle {
                #[cfg(debug_assertions)]
                println!("Nic {} wakes up", nic.address);
            }
            nic.idle = false;
            for m in q.iter() {
                nic.code.input.push(m.x);
                nic.code.input.push(m.y);
            }
        }
        let queue_empty = nic.code.input.is_empty();
        if queue_empty {
            nic.code.input.push(-1);
        }
        nic.code.step().expect("step");
        if queue_empty {
            if nic.code.input.is_empty() {
                nic.idle = true;
            } else {
                nic.code.input.clear();
            }
        }
        if nic.code.output.len() >= 3 {
            nic.idle = false;
            let to = nic.code.output.remove(0);
            let x = nic.code.output.remove(0);
            let y = nic.code.output.remove(0);
            #[cfg(debug_assertions)]
            println!("Message from {} to {}: {} {}", nic.address, to, x, y);
            result.entry(to)
                .or_insert_with(Vec::new)
                .push(Message{x, y});
        }
    }
    result
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let program = Program::load(args.get(1));
    let mut nics: Vec<NIC> = (0..50)
        .map(|a| NIC::new(&program, a))
        .collect();

    let mut messages = HashMap::new();
    let mut nat = loop {
        messages = step(&mut nics, messages);
        if let Some(q) = messages.get_mut(&255) {
            break q.remove(0);
        }
    };
    println!("Result: {}", nat.y);
    let mut last_sent_nat = nat.y-1;
    let result2 = loop {
        if nics.iter().all(|nic| nic.idle) {
            #[cfg(debug_assertions)]
            println!("Sending NAT {}", nat.y);
            if nat.y == last_sent_nat {
                break last_sent_nat;
            }
            messages.entry(0)
                .or_insert_with(Vec::new)
                .push(nat);
            last_sent_nat = nat.y;
        }
        messages = step(&mut nics, messages);
        if let Some(q) = messages.get_mut(&255) {
            nat = q.pop().unwrap();
            q.clear();
        }
    };
    println!("Result2: {}", result2);
}
