use std::fs;
use std::iter;

use aoc2019::ext_gcd::ext_gcd;

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

fn binom(n: usize, k: usize) -> usize {
    if n < k {
        return 0;
    }
    let mut num = 1;
    let mut den = 1;
    for i in 1..=usize::min(k, n - k) {
        num *= n + 1 - i;
        den *= i;
    }
    num / den
}

// n choose k, mod p assuming p is prime.
// This is Lucas's theorem.
fn binom_p(mut n: usize, mut k: usize, p: usize) -> usize {
    let mut res = 1;
    while n > 0 || k > 0 {
        res *= binom(n % p, k % p);
        res %= p;
        n /= p;
        k /= p;
    }
    res
}

// Chinese Remainder Theorem, for two coprime moduli
// Solve x = a mod m and x = b mod n, for 0<=x<=m*n
fn crt(a: i64, m: i64, b: i64, n: i64) -> i64 {
    let (gcd, u, v) = ext_gcd(m, n);
    if gcd != 1 {
        panic!("crt: the arguments m={}, n={} were not coprime", m, n);
    }
    let res = (a * v * n + b * u * m) % (m * n);
    if res < 0 {
        res + m * n
    } else {
        res
    }
}

// n choose k, modulo 10
fn binom_10(n: usize, k: usize) -> usize {
    let b2 = binom_p(n, k, 2);
    let b5 = binom_p(n, k, 5);
    crt(b2 as i64, 2, b5 as i64, 5) as usize
}

fn main() {
    let dat = fs::read_to_string("../data/day16").unwrap();
    let dat: Vec<i64> = dat
        .trim()
        .chars()
        .map(|c| c.to_string().parse::<i64>().unwrap())
        .collect();
    let pattern = [0, 1, 0, -1];
    let num_fft = 100;

    let data = dat.clone();
    let mut ffts = iter::successors(Some(data), |d| Some(fft(d, &pattern[..])));
    let parta_digits = &ffts.nth(num_fft).unwrap()[0..8];
    print!("part a, {}th iteration gives: ", num_fft);
    for d in parta_digits {
        print!("{}", d);
    }
    println!("");

    /* Part b: we want a particular 8 digits a decent way into the 100th fft
     * of (the input repeated 10 thousand times).
     * At least in my input, this is well into the second half.
     * Note that since the pattern for the nth output digit is 0^{n-1}1^n...,
     * we have that in the second half we are just taking the sum of the tails.
     * Note that we can work mod 10, as we don't have to worry about truncating negative numbers.
     * So we can work on the tail of the list starting at the offset.
     * Consider the list x0,x1,...,xk, and the map f(x) = (x0+x1+...+xk, x1+...+xk, ..., xk)
     * (i.e. a generic input, and the fft within the second half).
     * Then f^n(x)_i = sum(j=i..n , binom(n-1+j,j)*x_j).
     * To see this, consider the tree of additions we are doing to find f^n(x)_i.
     * Each path terminates in a leaf which is one of the x_j, and the nodes are the f^m(x)_j for
     * m=n,n-1,...,0, and some increasing j starting at i.
     * Considering the coefficient of x_k and the grid of m=n..0, j=i..k, we are tracing out
     * staircase paths, from (n,i) to (1,k) to (0,k). There are binom(n+k-i-1,n-1) of these.
     */
    let mut off = 0;
    for i in 0..7 {
        off *= 10;
        off += dat[i];
    }
    let off = off as usize;
    let datb_len = 10000 * dat.len();
    if !(off * 2 >= datb_len) {
        panic!("part b: offset is not in the second-half of the repeated input!");
    }
    print!("part b: ");
    for i in 0..8 {
        let mut out_i = 0;
        for k in i..datb_len - off {
            let x_k = dat[(off + k) % dat.len()] as usize;
            let b = binom_10(num_fft + k - i - 1, num_fft - 1);
            out_i += b * x_k;
            out_i %= 10;
        }
        print!("{}", out_i);
    }
    println!("");
}
