use std::fs;
use std::iter;

// The units digit, where truncate(-17) = 7
fn truncate(n: i64) -> i64 {
    n.abs() % 10
}

// Flawed Frequency Transmission
// (not Fast Fourier Transform!)
fn fft(signal: &Vec<i64>, pattern: &[i64]) -> Vec<i64> {
    let mut output = Vec::with_capacity(signal.len());
    for i in 0..signal.len() {
        let p = pattern
            .iter()
            .flat_map(|d| iter::repeat(d).take(i + 1))
            .cycle()
            .skip(1);
        let n = signal.iter().zip(p).map(|(d, p)| d * p).sum();
        output.push(truncate(n));
    }
    output
}

fn main() {
    let dat = fs::read_to_string("../data/day16").unwrap();
    let dat = dat
        .trim()
        .chars()
        .map(|c| c.to_string().parse::<i64>().unwrap())
        .collect();
    let pattern = [0, 1, 0, -1];

    let mut ffts = iter::successors(Some(dat), |d| Some(fft(d, &pattern[..])));
    let parta_digits = &ffts.nth(100).unwrap()[0..8];
    print!("part a, 100th iteration gives: ");
    for d in parta_digits {
        print!("{}", d);
    }
    println!("");
}
