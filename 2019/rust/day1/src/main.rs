use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

fn main() {
    let path = Path::new("../../data/day1a");
    let file = File::open(path).unwrap();
    let lines = BufReader::new(file).lines();
    let tot: (i32, i32) = lines.fold((0, 0), |acc, l| {
        let f = to_fuel(l.unwrap());
        (acc.0 + f.0, acc.1 + f.1)
    });

    println!("Total Fuel needed (simplistic, part a): {}", tot.0);
    println!("Total Fuel needed (tyrannical, part b): {}", tot.1);
}

fn to_fuel(s: String) -> (i32, i32) {
    let mass: i32 = s.parse().unwrap();
    let simplistic = mass / 3 - 2;
    let mut tmp = simplistic;
    let mut tyrannical = 0;
    while tmp > 0 {
        tyrannical += tmp;
        tmp = tmp / 3 - 2;
    }
    (simplistic, tyrannical)
}
