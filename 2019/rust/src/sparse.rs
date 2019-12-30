use std::collections::HashMap;
use std::convert::TryInto;
use std::hash::Hash;
use std::ops::{Add, Div, Index, IndexMut, Mul, Rem, Sub};
use std::str::FromStr;

use num::{one, zero, BigInt, Num, One, Zero};
use num_traits::cast::ToPrimitive;

use crate::intcode::Vector;

#[derive(Clone)]
pub struct Sparse<I: Eq + Hash, T> {
    default: T,
    data: HashMap<I, T>,
}

// This will iterate over the non-default entries
impl<I: Eq + Hash, T: Eq + Clone> Sparse<I, T> {
    pub fn iter<'a>(&'a self) -> impl 'a + Iterator<Item = (&I, &T)> {
        self.data.iter().filter(move |(_, v)| **v != self.default)
    }
}

impl<I: Eq + Hash, T> Sparse<I, T> {
    pub fn new(default: T, data: HashMap<I, T>) -> Self {
        Sparse { default, data }
    }
}

impl<I: Eq + Hash, T> Index<I> for Sparse<I, T> {
    type Output = T;
    fn index(&self, i: I) -> &T {
        self.data.get(&i).unwrap_or(&self.default)
    }
}
impl<I: Eq + Hash, T: Clone> IndexMut<I> for Sparse<I, T> {
    fn index_mut(&mut self, i: I) -> &mut T {
        self.data.entry(i).or_insert(self.default.clone())
    }
}

impl<T: Clone> Vector<T> for Sparse<usize, T> {}

// need a wrapper for TryInto<usize>, which then requires me to write all the trivial newtype trait
// implementations. Sigh.
#[derive(Clone, Debug)]
pub struct BigIntWrapper {
    big_int: BigInt,
}
#[derive(Debug)]
pub enum BigIntConvertErr {
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
impl From<usize> for BigIntWrapper {
    fn from(f: usize) -> Self {
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
