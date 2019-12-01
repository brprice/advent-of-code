use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

fn main() {
    let path = Path::new("../../data/day1a");
    let file = File::open(path).unwrap();
    let lines = BufReader::new(file).lines();
    let tot : i32 = lines.map(|l| to_fuel(l.unwrap())).sum();

    println!("Total Fuel needed: {}",tot);
} 

fn to_fuel(s : String) -> i32 {
    let mass : i32 = s.parse().unwrap();
    mass/3 - 2
}
