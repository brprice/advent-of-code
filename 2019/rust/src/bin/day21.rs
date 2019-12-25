use aoc2019::intcode::*;
use aoc2019::sparse::*;

/* Today was a "jump over the holes" game - you either move forward one square, or jump over three.
 * It seems impossible to ensure success (i.e. not falling into a hole) with finite look-ahead, but
 * these strategies work for my inputs */

fn main() {
    let mem: Sparse<usize, i64> = read_intcode_sparse("../data/day21");
    let mach = IC::new(0, mem);
    let mut macha = mach.clone();
    // jump if there is land 4 tiles away, and a hole before that
    let inpa = "NOT A T
OR T J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
";
    let mut inpa = inpa.chars().map(|c| c as i64);
    let outa = macha.run(&mut inpa);
    println!("{}", outa.last().unwrap());

    let mut machb = mach;
    // Jump if you can land, it isn't immediate doom, and you must soon
    // (i.e. an optimised version of part a, plus a bit more look-ahead)
    // D && (E || H) && (!A || !B || !C)
    let inpb = "OR E J
OR H J
AND D J
NOT T T
AND A T
AND B T
AND C T
NOT T T
AND T J
RUN
";

    let mut inpb = inpb.chars().map(|c| c as i64);
    let outb = machb.run(&mut inpb);
    println!("{}", outb.last().unwrap());
}
