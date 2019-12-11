use std::collections::HashMap;
use std::iter::once;

use num::{one, zero, One};

use aoc2019::intcode::*;
use aoc2019::sparse::*;

fn main() {
    let mem: HashMap<usize, BigIntWrapper> = read_intcode("../data/day9")
        .iter()
        .cloned()
        .enumerate()
        .collect();
    let mach = IC::new(0, Sparse::new(zero(), mem));
    let mut input = once(one());
    let output: Vec<_> = mach.clone().run(&mut input).collect();

    if output.len() != 1 {
        println!("part a: errors detected: {:?}", output);
    } else {
        println!("part a: {}", output[0]);
    }

    let mut inputb = once(BigIntWrapper::one() + one());
    let outputb = mach.clone().run(&mut inputb).next().unwrap();
    println!("part b: {}", outputb);
}
