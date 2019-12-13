extern crate itertools;

use itertools::Itertools;
use std::collections::HashMap;
use std::iter::empty;

use aoc2019::intcode::*;
use aoc2019::sparse::*;

fn main() {
    let mem: Sparse<usize, i64> = read_intcode_sparse("../data/day13");
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
}
