/* Let's try brute force: generate all increasing digit sequences in a range (as an iterator), and
 * filter that, then take the length
 */

use std::iter::successors;

type Digit = u8;

fn succ(ds: &Vec<Digit>) -> Option<Vec<Digit>> {
    if ds.is_empty() || ds[0] == 9 {
        return None;
    }
    let mut ds = ds.clone();
    let mut first9 = 0;
    while first9 < ds.len() && ds[first9] != 9 {
        first9 += 1;
    }
    ds[first9 - 1] += 1;
    let d = ds[first9 - 1];
    for i in first9..ds.len() {
        ds[i] = d;
    }
    return Some(ds);
}

fn make_asc(mut ds: Vec<Digit>) -> Vec<Digit> {
    if ds.is_empty() {
        return ds;
    }
    let mut first_dec = None;
    for i in 0..ds.len() - 1 {
        if ds[i] > ds[i + 1] {
            first_dec = Some(i);
            break;
        }
    }
    match first_dec {
        None => {}
        Some(j) => {
            for i in j + 1..ds.len() {
                ds[i] = ds[j];
            }
        }
    }
    return ds;
}

fn digits(mut n: u32) -> Vec<u8> {
    let mut out = Vec::new();
    while n > 0 {
        let r = (n % 10) as u8;
        out.push(r);
        n /= 10;
    }
    out.reverse();
    return out;
}

fn asc_digits(min: Vec<Digit>, max: Vec<Digit>) -> impl Iterator<Item = Vec<u8>> {
    assert_eq!(min.len(), max.len(), "ascDigits: unequal len");
    let min = make_asc(min);
    successors(if min >= max { None } else { Some(min) }, move |d| {
        let e = succ(d)?;
        if e >= max {
            None
        } else {
            Some(e)
        }
    })
}

fn main() {
    let lb = digits(240298);
    let ub = digits(784956);
    let good = |ds: &Vec<u8>| ds.windows(2).any(|w| w[0] == w[1]);
    let parta = asc_digits(lb, ub).filter(good).count();
    println!("parta: {}", parta);
}
