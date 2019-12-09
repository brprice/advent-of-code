use std::iter::empty;

use aoc2019::intcode::*;

#[derive(Debug)]
enum Err {
    IO(std::io::Error),
    UTF8(std::string::FromUtf8Error),
    Parse(std::num::ParseIntError),
}

impl From<std::io::Error> for Err {
    fn from(e: std::io::Error) -> Err {
        Err::IO(e)
    }
}

impl From<std::string::FromUtf8Error> for Err {
    fn from(e: std::string::FromUtf8Error) -> Err {
        Err::UTF8(e)
    }
}

impl From<std::num::ParseIntError> for Err {
    fn from(e: std::num::ParseIntError) -> Err {
        Err::Parse(e)
    }
}

fn main() -> Result<(), Err> {
    let ops = read_intcode("../data/day2");
    let parta = run(12, 2, ops.clone());
    println!("part a: output for 1202 run is {}", parta);

    let target = 19690720;
    println!(
        "part b: now searching for noun/verb pair giving output of {}",
        target
    );
    'n: for n in 0..=99 {
        for v in 0..=99 {
            if run(n, v, ops.clone()) == target {
                println!("part b: found noun = {}, verb = {}", n, v);
                println!("part b: thus answer is {}", 100 * n + v);
                break 'n;
            }
        }
    }

    Ok(())
}

fn run(noun: i32, verb: i32, mut ops: Vec<i32>) -> i32 {
    ops[1] = noun;
    ops[2] = verb;
    let mut mach = IC::new(0, ops);
    mach.run(&mut empty()).count();
    mach.mem[0]
}
