extern crate itertools;

use itertools::Itertools;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::iter::empty;

use aoc2019::intcode::*;
use aoc2019::sparse::*;

struct State {
    ball: (i64, i64),
    blocks: HashSet<(i64, i64)>,
    paddlex: i64,
    score: i64,
}

fn main() {
    let mem: Sparse<usize, i64> = read_intcode_sparse("../data/day13");
    let memb = mem.clone(); // for part b
    let mut mach = IC::new(0, mem);
    let mut inp = empty();
    let out = mach.run(&mut inp);

    let mut screen = HashMap::new();

    for mut o in out.chunks(3).into_iter() {
        let x = o.next().unwrap();
        let y = o.next().unwrap();
        let id = o.next().unwrap();
        screen.insert((x, y), id);
    }
    let num_blocks = screen.values().filter(|&&id| id == 2).count();
    println!("part a: {}", num_blocks);

    // for part b, we actually grab the score when the machine halts, rather
    // than after the last block is broken.
    let mut memb = memb;
    memb[0] = 2;
    let mut mach = IC::new(0, memb);
    let mut state = State {
        ball: (0, 0),
        blocks: HashSet::new(),
        paddlex: 0,
        score: 0,
    }; // garbage to start with
    loop {
        match run_to_i3o(mach) {
            I3O::Out(mach1, x, y, id) => {
                mach = mach1;
                if x == -1 && y == 0 {
                    state.score = id;
                } else {
                    match id {
                        0 /* empty */ => {state.blocks.remove(&(x,y));},
                        2 /* block */ => {state.blocks.insert((x,y));},
                        3 /* paddle */ => state.paddlex=x,
                        4 /* ball */ => state.ball=(x,y),
                        _ => {},
                    }
                }
            }
            I3O::In(f) => {
                let input = match state.paddlex.cmp(&state.ball.0) {
                    Ordering::Less => 1,
                    Ordering::Equal => 0,
                    Ordering::Greater => -1,
                };
                mach = f(input);
            }
            I3O::Halt => break,
        }
    }

    println!("part b: {}", state.score);
}

pub enum I3O {
    Out(IC<i64, Sparse<usize, i64>>, i64, i64, i64),
    In(Box<dyn FnOnce(i64) -> IC<i64, Sparse<usize, i64>>>),
    Halt,
}
// let's assume inputs/halts never happen in the middle of a block of three outputs
fn run_to_i3o(ic: IC<i64, Sparse<usize, i64>>) -> I3O {
    match ic.run_to_io() {
        IO::Out(ic1, o1) => match ic1.run_to_io() {
            IO::Out(ic2, o2) => match ic2.run_to_io() {
                IO::Out(ic3, o3) => I3O::Out(ic3, o1, o2, o3),
                _ => panic!("run_to_i3o assumptions invalidated"),
            },
            _ => panic!("run_to_i3o assumptions invalidated"),
        },
        IO::In(f) => I3O::In(f),
        IO::Halt => I3O::Halt,
    }
}
