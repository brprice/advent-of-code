use std::collections::{HashMap, HashSet};

use num::{one, zero, One, Zero};

use aoc2019::intcode::*;
use aoc2019::sparse::*;

#[derive(Copy, Clone, PartialEq, Eq)]
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
    (y, -x)
}

fn right((x, y): (isize, isize)) -> (isize, isize) {
    (-y, x)
}

fn main() {
    let mem: Sparse<usize, BigIntWrapper> = read_intcode_sparse("../data/day11");
    let mach = IC::new(0, mem);
    let origin = (0, 0);
    let up = (0, -1);
    let mut ship: Sparse<(isize, isize), Colour> = Sparse::new(Colour::Black, HashMap::new());
    let painted = paint(mach.clone(), origin, up, &mut ship);

    println!("part a: total distinct panels painted: {}", painted.len());

    let mut ship: Sparse<(isize, isize), Colour> = Sparse::new(Colour::Black, HashMap::new());
    ship[(0, 0)] = Colour::White;
    paint(mach, origin, up, &mut ship);

    println!("part b: painting is");
    let (minx, miny, maxx, maxy) =
        ship.iter()
            .fold((0, 0, 0, 0), |(mix, miy, max, may), ((x, y), _)| {
                (
                    std::cmp::min(mix, *x),
                    std::cmp::min(miy, *y),
                    std::cmp::max(max, *x),
                    std::cmp::max(may, *y),
                )
            });

    for y in miny..=maxy {
        for x in minx..=maxx {
            if ship[(x, y)] == Colour::Black {
                print!(" ");
            } else {
                print!("#")
            }
        }
        print!("\n");
    }
}

fn paint(
    mut mach: IC<BigIntWrapper, Sparse<usize, BigIntWrapper>>,
    mut pos: (isize, isize),
    mut dir: (isize, isize),
    ship: &mut Sparse<(isize, isize), Colour>,
) -> HashSet<(isize, isize)> {
    let mut is_painting = true;

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
    painted
}
