use std::collections::{HashMap, HashSet};

use num::{one, zero, One, Zero};

use aoc2019::intcode::*;
use aoc2019::sparse::*;

#[derive(Copy, Clone)]
enum Colour {
    Black,
    White,
}

impl Into<BigIntWrapper> for Colour {
    fn into(self) -> BigIntWrapper {
        match self {
            Colour::Black => zero(),
            Colour::White => one(),
        }
    }
}

fn plus((a, b): (isize, isize), (c, d): (isize, isize)) -> (isize, isize) {
    (a + c, b + d)
}

fn left((x, y): (isize, isize)) -> (isize, isize) {
    (-y, x)
}

fn right((x, y): (isize, isize)) -> (isize, isize) {
    (y, -x)
}

fn main() {
    let mem: HashMap<usize, BigIntWrapper> = read_intcode("../data/day11")
        .iter()
        .cloned()
        .enumerate()
        .collect();
    let mut mach = IC::new(0, Sparse::new(zero(), mem));
    let mut pos = (0, 0);
    let mut dir = (0, 1);
    let mut is_painting = true;
    let mut ship: Sparse<(isize, isize), Colour> = Sparse::new(Colour::Black, HashMap::new());

    // We don't want to look inside the sparse matrix to see how many elts are stored in that
    // hashset. This would break the abstraction barrier, and we wish to remain agnostic to the
    // implementation detail of what happens when we write the default to a location.
    // (This could store the default in the hashmap, or it could drop that elt from the map.)
    // So, let's track this ourselves.
    let mut painted = HashSet::new();

    loop {
        match mach.run_to_io() {
            IO::Out(new_mach, out) => {
                mach = new_mach;
                if is_painting {
                    painted.insert(pos);
                    if out.is_zero() {
                        ship[pos] = Colour::Black
                    } else if out.is_one() {
                        ship[pos] = Colour::White
                    } else {
                        panic!("Unknown paint output {}", out)
                    }
                } else {
                    if out.is_zero() {
                        dir = left(dir);
                        pos = plus(pos, dir)
                    } else if out.is_one() {
                        dir = right(dir);
                        pos = plus(pos, dir)
                    } else {
                        panic!("Unknown direction output {}", out)
                    }
                }
                is_painting = !is_painting;
            }
            IO::In(cont) => mach = cont(ship[pos].clone().into()),
            IO::Halt => break,
        }
    }
    println!("part a: total distinct panels painted: {}", painted.len());
}
