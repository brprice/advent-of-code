use aoc2019::intcode::*;
use aoc2019::sparse::*;

fn main() {
    let mem: Sparse<usize, i64> = read_intcode_sparse("../data/day19");
    let mach = IC::new(0, mem.clone());
    // The machine takes two inputs, gives one output, then halts

    let in_beam = |(x, y)| {
        let mut inp = vec![x, y].into_iter();
        let mut m = mach.clone();
        let mut out = m.run(&mut inp);
        let o = out.next().unwrap();
        o == 1
    };
    let mut t = (0, 0); // for part b
    let mut n = 0;
    for y in 0..50 {
        for x in 0..50 {
            if in_beam((x, y)) {
                if t == (0, 0) {
                    t = (x, y);
                }
                n += 1;
            }
        }
    }
    println!("part a: {}", n);

    /* part b: As the beam gets wider (presumably monotonically) moving away from the origin,
     * if we have two points s and t at the bottom-left and top-right of a square:
     *  +---t
     *  |   |
     *  s---+
     *  then the whole square is inside the beam iff both s and t are.
     *  Thus the algorithm is: scan along one side of the beam (t), and find the first time
     *  that the opposite corner of our square is also in the beam.
     *  We start at the first bit of beam that is not the origin, as the beam seems to be
     *  connected, except for an isolated point (0,0)
     */
    // t has been initialised above
    let mk_s = |(x, y)| (x - 99, y + 99);
    let mut s = mk_s(t);
    while s.0 < 0 || !in_beam(s) {
        // find next t
        t = (t.0 + 1, t.1);
        while !in_beam(t) {
            t = (t.0, t.1 + 1);
        }
        s = mk_s(t);
    }
    let x = s.0;
    let y = t.1;
    println!("part b: {}", 10000 * x + y);
}
