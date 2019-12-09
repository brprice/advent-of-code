use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

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
    let path = Path::new("../data/day2");
    let file = File::open(path)?;
    let ops: Vec<u32> = BufReader::new(file)
        .split(b',')
        .map(|x| Ok(String::from_utf8(x?)?.trim().parse()?))
        .collect::<Result<_, Err>>()?;

    let parta = run(12, 2, &mut ops.clone());

    println!("part a: output for 1202 run is {}", parta);

    let target = 19690720;
    println!(
        "part b: now searching for noun/verb pair giving output of {}",
        target
    );
    'n: for n in 0..=99 {
        for v in 0..=99 {
            if run(n, v, &mut ops.clone()) == target {
                println!("part b: found noun = {}, verb = {}", n, v);
                println!("part b: thus answer is {}", 100 * n + v);
                break 'n;
            }
        }
    }

    Ok(())
}

// who needs error handling?
fn run(noun: u32, verb: u32, ops: &mut Vec<u32>) -> u32 {
    ops[1] = noun;
    ops[2] = verb;
    let mut ip = 0;
    let mut i = ops[ip];
    while i != 99 {
        let d = ops[ip + 3] as usize;
        let a = ops[ip + 1] as usize;
        let b = ops[ip + 2] as usize;
        match i {
            1 => ops[d] = ops[a] + ops[b],
            2 => ops[d] = ops[a] * ops[b],
            _ => panic!("Invalid opcode: {}", i),
        }
        ip += 4;
        i = ops[ip];
    }
    ops[0]
}
