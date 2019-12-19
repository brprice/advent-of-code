use aoc2019::intcode::*;
use aoc2019::sparse::*;

fn main() {
    let mem: Sparse<usize, i64> = read_intcode_sparse("../data/day19");
    let mach = IC::new(0, mem.clone());
    // The machine takes two inputs, gives one output, then halts

    let mut n = 0;
    for y in 0..50 {
        for x in 0..50 {
            let mut inp = vec![x, y].into_iter();
            let mut m = mach.clone();
            let mut out = m.run(&mut inp);
            let o = out.next().unwrap();
            if o == 1 {
                n += 1;
            }
        }
    }
    println!("part a: {}", n);
}
