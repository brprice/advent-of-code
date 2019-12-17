use aoc2019::intcode::*;
use aoc2019::sparse::*;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::iter::empty;

fn main() {
    let mach: Sparse<usize, i64> = read_intcode_sparse("../data/day17");
    let mut mach = IC::new(0, mach);
    let mut no_in = empty();
    let scaffold = mach.run(&mut no_in);

    println!("part a, the scaffold looks like:");

    let mut x = -1;
    let mut y = 0;
    let mut scaf = HashSet::new();
    for c in scaffold {
        x += 1;
        match u8::try_from(c).map(char::from) {
            Ok(d) => {
                print!("{}", d);
                match d {
                    '\n' => {
                        x = -1;
                        y += 1;
                    }
                    '.' => {}
                    'X' => {}
                    '#' => {
                        scaf.insert((x, y));
                    }
                    '^' => {
                        scaf.insert((x, y));
                    }
                    '>' => {
                        scaf.insert((x, y));
                    }
                    'v' => {
                        scaf.insert((x, y));
                    }
                    '<' => {
                        scaf.insert((x, y));
                    }
                    _ => panic!("unexpected character: {}", d),
                }
            }
            Err(e) => panic!("could not convert {} to a char: {}", c, e),
        }
    }

    let mut align_param_sum = 0;
    for (x, y) in &scaf {
        let up = scaf.contains(&(x + 1, *y));
        let down = scaf.contains(&(x - 1, *y));
        let left = scaf.contains(&(*x, y - 1));
        let right = scaf.contains(&(*x, y + 1));
        if up && down && left && right {
            // this is an intersection
            align_param_sum += x * y;
        }
    }
    println!("part a: sum of alignment parameters: {}", align_param_sum);
}
