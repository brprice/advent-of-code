use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::once;
use std::ops::{Add, Div, Index, IndexMut, Mul, Rem, Sub};
use std::str::FromStr;

use num::{one, zero, BigInt, Num, One, Zero};
use num_traits::cast::ToPrimitive;

use aoc2019::intcode::*;

struct Sparse<T> {
    default: T,
    data: HashMap<usize, T>,
}

impl<T> Sparse<T> {
    fn new(default: T, data: HashMap<usize, T>) -> Self {
        Sparse { default, data }
    }
}

impl<T> Index<usize> for Sparse<T> {
    type Output = T;
    fn index(&self, i: usize) -> &T {
        self.data.get(&i).unwrap_or(&self.default)
    }
}
impl<T: Clone> IndexMut<usize> for Sparse<T> {
    fn index_mut(&mut self, i: usize) -> &mut T {
        self.data.entry(i).or_insert(self.default.clone())
    }
}

impl<T: Clone> Vector<T> for Sparse<T> {}

// need a wrapper for TryInto<usize>, which then requires me to write all the trivial newtype trait
// implementations. Sigh.
#[derive(Clone, Debug)]
struct BigIntWrapper {
    big_int: BigInt,
}
#[derive(Debug)]
enum BigIntConvertErr {
    BigIntConvertErr,
}
impl TryInto<usize> for BigIntWrapper {
    type Error = BigIntConvertErr;
    fn try_into(self) -> Result<usize, BigIntConvertErr> {
        self.big_int
            .to_usize()
            .ok_or(BigIntConvertErr::BigIntConvertErr)
    }
}
impl FromStr for BigIntWrapper {
    type Err = <BigInt as FromStr>::Err;
    fn from_str(s: &str) -> Result<BigIntWrapper, Self::Err> {
        BigInt::from_str(s).map(|big_int| BigIntWrapper { big_int })
    }
}
impl Add for BigIntWrapper {
    type Output = BigIntWrapper;
    fn add(self, rhs: Self) -> Self {
        BigIntWrapper {
            big_int: self.big_int + rhs.big_int,
        }
    }
}
impl Sub for BigIntWrapper {
    type Output = BigIntWrapper;
    fn sub(self, rhs: Self) -> Self {
        BigIntWrapper {
            big_int: self.big_int - rhs.big_int,
        }
    }
}
impl Mul for BigIntWrapper {
    type Output = BigIntWrapper;
    fn mul(self, rhs: Self) -> Self {
        BigIntWrapper {
            big_int: self.big_int * rhs.big_int,
        }
    }
}
impl Div for BigIntWrapper {
    type Output = BigIntWrapper;
    fn div(self, rhs: Self) -> Self {
        BigIntWrapper {
            big_int: self.big_int / rhs.big_int,
        }
    }
}
impl Rem for BigIntWrapper {
    type Output = BigIntWrapper;
    fn rem(self, rhs: Self) -> Self {
        BigIntWrapper {
            big_int: self.big_int % rhs.big_int,
        }
    }
}
impl Zero for BigIntWrapper {
    fn zero() -> Self {
        BigIntWrapper { big_int: zero() }
    }
    fn is_zero(&self) -> bool {
        BigInt::is_zero(&self.big_int)
    }
}
impl One for BigIntWrapper {
    fn one() -> Self {
        BigIntWrapper { big_int: one() }
    }
}
impl PartialEq for BigIntWrapper {
    fn eq(&self, rhs: &Self) -> bool {
        self.big_int == rhs.big_int
    }
}
impl Num for BigIntWrapper {
    type FromStrRadixErr = <BigInt as Num>::FromStrRadixErr;
    fn from_str_radix(s: &str, r: u32) -> Result<Self, Self::FromStrRadixErr> {
        BigInt::from_str_radix(s, r).map(|big_int| BigIntWrapper { big_int })
    }
}
impl PartialOrd for BigIntWrapper {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        BigInt::partial_cmp(&self.big_int, &rhs.big_int)
    }
}
impl From<u8> for BigIntWrapper {
    fn from(f: u8) -> Self {
        BigIntWrapper {
            big_int: BigInt::from(f),
        }
    }
}
impl std::fmt::Display for BigIntWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        self.big_int.fmt(f)
    }
}
fn main() {
    let mem: HashMap<usize, BigIntWrapper> = read_intcode("../data/day9")
        .iter()
        .cloned()
        .enumerate()
        .collect();
    let mut mach = IC::new(0, Sparse::new(zero(), mem));
    let mut input = once(one());
    let output: Vec<_> = mach.run(&mut input).collect();

    if output.len() != 1 {
        println!("part a: errors detected: {:?}", output);
    } else {
        println!("part a: {}", output[0]);
    }
}
