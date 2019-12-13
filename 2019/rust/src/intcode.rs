extern crate num;

use num::{zero, Num, Zero};
use std::convert::TryInto;
use std::fmt::{Debug, Display};
use std::fs::read_to_string;
use std::marker::PhantomData;
use std::ops::IndexMut;
use std::path::Path;
use std::str::FromStr;

use crate::sparse::Sparse;

pub trait Vector<T>
where
    Self: IndexMut<usize, Output = T>,
{
}

impl<T> Vector<T> for Vec<T> {}

#[derive(Clone)]
pub struct IC<T, C>
where
    C: Vector<T>,
{
    pub ip: usize,
    pub rel_base: usize,
    pub mem: C,
    phantom: PhantomData<T>,
}

pub fn read_intcode<P: AsRef<Path>, T: FromStr>(path: P) -> Vec<T>
where
    T::Err: Debug,
{
    // we trust the input, so just unwrap everything instead of doing error handling
    let data = read_to_string(path).expect("couldn't read file");
    data.trim()
        .split(',')
        .map(|x| x.parse().expect("couldn't parse"))
        .collect()
}

pub fn read_intcode_sparse<P: AsRef<Path>, T: FromStr>(path: P) -> Sparse<usize, T>
where
    T::Err: Debug,
{
    let mem = read_intcode(path).into_iter().enumerate().collect();
    let zero = "0".parse().expect("couldn't parse '0'");
    Sparse::new(zero, mem)
}

enum Output<T> {
    Cont,
    Out(T),
}
enum Mode {
    Pos,
    Imm,
    Rel,
}
enum Op {
    Add(Mode, Mode, Mode),
    Mul(Mode, Mode, Mode),
    In(Mode),
    Out(Mode),
    Jit(Mode, Mode),
    Jif(Mode, Mode),
    Lt(Mode, Mode, Mode),
    Eq(Mode, Mode, Mode),
    RB(Mode),
    Halt,
}

impl<T: Zero, C: Vector<T>> IC<T, C> {
    pub fn new(ip: usize, mem: C) -> IC<T, C> {
        IC {
            ip,
            rel_base: zero(),
            mem,
            phantom: PhantomData,
        }
    }
}
impl<T, C: Vector<T>> IC<T, C>
where
    T: Clone,
    T: TryInto<usize>,
    T: From<u8>,
    T: Num,
    T: PartialOrd,
    T: Display,
    <T as TryInto<usize>>::Error: Debug,
{
    fn get_op_mode(&self) -> Op {
        let i = self.mem[self.ip].clone();
        let op = i.clone() % T::from(100);
        let mut ams = i / T::from(100);
        let mut modes = std::iter::from_fn(|| {
            let m = ams.clone() % T::from(10);
            ams = ams.clone() / T::from(10);
            match m.try_into().expect("Bad mode") {
                0 => Some(Mode::Pos),
                1 => Some(Mode::Imm),
                2 => Some(Mode::Rel),
                m => panic!("Unrecognised mode: {}", m),
            }
        });
        match op.try_into().expect("Bad opcode") {
            1 => Op::Add(
                modes.next().unwrap(),
                modes.next().unwrap(),
                modes.next().unwrap(),
            ),
            2 => Op::Mul(
                modes.next().unwrap(),
                modes.next().unwrap(),
                modes.next().unwrap(),
            ),
            3 => Op::In(modes.next().unwrap()),
            4 => Op::Out(modes.next().unwrap()),
            5 => Op::Jit(modes.next().unwrap(), modes.next().unwrap()),
            6 => Op::Jif(modes.next().unwrap(), modes.next().unwrap()),
            7 => Op::Lt(
                modes.next().unwrap(),
                modes.next().unwrap(),
                modes.next().unwrap(),
            ),
            8 => Op::Eq(
                modes.next().unwrap(),
                modes.next().unwrap(),
                modes.next().unwrap(),
            ),
            9 => Op::RB(modes.next().unwrap()),
            99 => Op::Halt,
            op => panic!("Unrecognised opcode: {}", op),
        }
    }

    fn read(&self, a: usize, m: Mode) -> T {
        let b = self.mem[a].clone();
        match m {
            Mode::Pos => self.mem[b.try_into().expect("read bad index")].clone(),
            Mode::Imm => b,
            Mode::Rel => {
                let p = if b >= zero() {
                    self.rel_base + b.try_into().expect("read bad index")
                } else {
                    self.rel_base - (T::zero() - b).try_into().expect("read bad index")
                };
                self.mem[p as usize].clone()
            }
        }
    }

    fn write_param(&mut self, p: usize, m: Mode, v: T) {
        let q = self.mem[p].clone();
        match m {
            Mode::Pos => self.mem[q.try_into().expect("write: bad param index")] = v,
            Mode::Rel => {
                if q >= zero() {
                    self.mem
                        [self.rel_base.clone() + q.try_into().expect("write: bad param index")] = v
                } else {
                    self.mem[self.rel_base.clone()
                        - (T::zero() - q).try_into().expect("write: bad param index")] = v
                }
            }
            Mode::Imm => panic!("write_param: can't write to an Imm mode"),
        }
    }

    fn run_op2<F>(&mut self, f: F, am: Mode, bm: Mode, cm: Mode)
    where
        F: FnOnce(T, T) -> T,
    {
        let a = self.read(self.ip + 1, am);
        let b = self.read(self.ip + 2, bm);
        self.write_param(self.ip + 3, cm, f(a, b));
        self.ip += 4;
    }

    fn run_in1(&mut self, input: T, m: Mode) {
        self.write_param(self.ip + 1, m, input);
        self.ip += 2;
    }

    fn run_in<I>(&mut self, input: &mut I, m: Mode)
    where
        I: Iterator<Item = T>,
    {
        match input.next() {
            Some(i) => self.run_in1(i, m),
            None => panic!("Input ran dry"),
        }
    }

    fn run_out(&mut self, m: Mode) -> T {
        let o = self.read(self.ip + 1, m);
        self.ip += 2;
        o
    }

    fn run_ji(&mut self, dir: bool, am: Mode, bm: Mode) {
        let a = self.read(self.ip + 1, am);
        let b = self.read(self.ip + 2, bm);
        if dir == (a != T::from(0)) {
            self.ip = b.try_into().expect("run_ji: bad jump destination");
        } else {
            self.ip += 3;
        }
    }

    fn run_rb(&mut self, m: Mode) {
        let off = self.read(self.ip + 1, m);
        if off >= zero() {
            self.rel_base += off.try_into().expect("run_rb: bad offset")
        } else {
            self.rel_base -= (T::zero() - off).try_into().expect("run_rb: bad offset")
        };
        self.ip += 2;
    }

    fn run1<I>(&mut self, input: &mut I) -> Option<Output<T>>
    where
        I: Iterator<Item = T>,
    {
        match self.get_op_mode() {
            Op::Add(am, bm, cm) => {
                self.run_op2(|x, y| x + y, am, bm, cm);
                Some(Output::Cont)
            }
            Op::Mul(am, bm, cm) => {
                self.run_op2(|x, y| x * y, am, bm, cm);
                Some(Output::Cont)
            }
            Op::In(m) => {
                self.run_in(input, m);
                Some(Output::Cont)
            }
            Op::Out(m) => Some(Output::Out(self.run_out(m))),
            Op::Jit(am, bm) => {
                self.run_ji(true, am, bm);
                Some(Output::Cont)
            }
            Op::Jif(am, bm) => {
                self.run_ji(false, am, bm);
                Some(Output::Cont)
            }
            Op::Lt(am, bm, cm) => {
                self.run_op2(|x, y| if x < y { T::one() } else { T::zero() }, am, bm, cm);
                Some(Output::Cont)
            }
            Op::Eq(am, bm, cm) => {
                self.run_op2(|x, y| if x == y { T::one() } else { T::zero() }, am, bm, cm);
                Some(Output::Cont)
            }
            Op::RB(m) => {
                self.run_rb(m);
                Some(Output::Cont)
            }
            Op::Halt => None,
        }
    }

    pub fn run<'a, I>(&'a mut self, input: &'a mut I) -> impl 'a + Iterator<Item = T>
    where
        I: Iterator<Item = T>,
    {
        std::iter::from_fn(move || self.run1(input)).filter_map(|o| match o {
            Output::Cont => None,
            Output::Out(a) => Some(a),
        })
    }
}

pub enum IO<T, C: Vector<T>> {
    Out(IC<T, C>, T),
    In(Box<dyn FnOnce(T) -> IC<T, C>>),
    Halt,
}

impl<T: 'static, C: 'static> IC<T, C>
where
    C: Vector<T>,
    T: Clone,
    T: TryInto<usize>,
    T: From<u8>,
    T: Num,
    T: PartialOrd,
    T: Display,
    <T as TryInto<usize>>::Error: Debug,
{
    pub fn run_to_io(mut self) -> IO<T, C> {
        loop {
            match self.get_op_mode() {
                Op::Add(am, bm, cm) => {
                    self.run_op2(|x, y| x + y, am, bm, cm);
                }
                Op::Mul(am, bm, cm) => {
                    self.run_op2(|x, y| x * y, am, bm, cm);
                }
                Op::In(m) => {
                    return IO::In(Box::new(|input| {
                        self.run_in1(input, m);
                        self
                    }))
                }
                Op::Out(m) => {
                    let o = self.run_out(m);
                    return IO::Out(self, o);
                }
                Op::Jit(am, bm) => {
                    self.run_ji(true, am, bm);
                }
                Op::Jif(am, bm) => {
                    self.run_ji(false, am, bm);
                }
                Op::Lt(am, bm, cm) => {
                    self.run_op2(|x, y| if x < y { T::one() } else { T::zero() }, am, bm, cm);
                }
                Op::Eq(am, bm, cm) => {
                    self.run_op2(|x, y| if x == y { T::one() } else { T::zero() }, am, bm, cm);
                }
                Op::RB(m) => {
                    self.run_rb(m);
                }
                Op::Halt => return IO::Halt,
            }
        }
    }
}
