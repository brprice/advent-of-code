use aoc2019::intcode::*;
use aoc2019::sparse::*;

fn main() {
    let mem: Sparse<usize, i64> = read_intcode_sparse("../data/day21");
    let mut mach = IC::new(0, mem);
    // jump if there is land 4 tiles away, and a hole before that
    let inp = "NOT A T
OR T J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
";
    let mut inp = inp.chars().map(|c| c as i64);
    let out = mach.run(&mut inp);
    println!("{}", out.last().unwrap());
}
