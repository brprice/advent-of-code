use aoc2019::intcode::*;

fn main() {
    let mem = read_intcode("../data/day5");
    let mut macha = IC { ip: 0, mem };
    let mut machb = macha.clone();

    let input = vec![1_i32];
    let mut input = input.iter().map(|x| *x);
    let mut out = macha.run(&mut input).skip_while(|x| *x == 0_i32);
    match out.next() {
        Some(out1) => match out.next() {
            None => println!("part a: {}", out1),
            Some(b) => println!(
                "part a, tests FAILED, had some zeros and then {},{},{:?}",
                out1,
                b,
                out.collect::<Vec<_>>()
            ),
        },
        None => println!("part a, unknown failure, we only output zeros"),
    }

    let input = vec![5_i32];
    let mut input = input.iter().map(|x| *x);
    let mut out = machb.run(&mut input);
    match out.next() {
        Some(res) => match out.next() {
            None => println!("part b: {}", res),
            Some(b) => println!(
                "part b, failure, multiple outputs: {},{},{:?}",
                res,
                b,
                out.collect::<Vec<_>>()
            ),
        },
        None => println!("part b, unknown failure, no output"),
    }
}
