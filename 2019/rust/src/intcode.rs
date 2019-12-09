extern crate num;

use num::Num;
use std::convert::TryInto;
use std::fmt::{Debug, Display};
use std::fs::read_to_string;
use std::marker::PhantomData;
use std::ops::IndexMut;
use std::path::Path;
use std::str::FromStr;

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
enum Output<T> {
    Cont,
    Out(T),
}
enum Mode {
    Pos,
    Imm,
}
enum Op {
    Add(Mode, Mode),
    Mul(Mode, Mode),
    In,
    Out(Mode),
    Jit(Mode, Mode),
    Jif(Mode, Mode),
    Lt(Mode, Mode),
    Eq(Mode, Mode),
    Halt,
}

impl<T, C: Vector<T>> IC<T, C> {
    pub fn new(ip: usize, mem: C) -> IC<T, C> {
        IC {
            ip,
            mem,
            phantom: PhantomData,
        }
    }
}
impl<T, C: Vector<T>> IC<T, C>
where
    T: Copy,
    T: TryInto<usize>,
    T: From<u8>,
    T: Num,
    T: PartialOrd,
    T: Display,
    <T as TryInto<usize>>::Error: Debug,
{
    fn get_op_mode(&self) -> Op {
        let i = self.mem[self.ip];
        let op = i % T::from(100);
        let mut ams = i / T::from(100);
        let mut modes = std::iter::from_fn(|| {
            let m = ams % T::from(10);
            ams = ams / T::from(10);
            match m.try_into().expect("Bad mode") {
                0 => Some(Mode::Pos),
                1 => Some(Mode::Imm),
                m => panic!("Unrecognised mode: {}", m),
            }
        });
        match op.try_into().expect("Bad opcode") {
            1 => Op::Add(modes.next().unwrap(), modes.next().unwrap()),
            2 => Op::Mul(modes.next().unwrap(), modes.next().unwrap()),
            3 => Op::In,
            4 => Op::Out(modes.next().unwrap()),
            5 => Op::Jit(modes.next().unwrap(), modes.next().unwrap()),
            6 => Op::Jif(modes.next().unwrap(), modes.next().unwrap()),
            7 => Op::Lt(modes.next().unwrap(), modes.next().unwrap()),
            8 => Op::Eq(modes.next().unwrap(), modes.next().unwrap()),
            99 => Op::Halt,
            _ => panic!("Unrecognised opcode: {}", op),
        }
    }

    fn read(&self, a: usize, m: Mode) -> T {
        let b = self.mem[a];
        match m {
            Mode::Pos => self.mem[b.try_into().expect("read bad index")],
            Mode::Imm => b,
        }
    }

    fn write_param(&mut self, p: usize, v: T) {
        let q = self.mem[p].try_into().expect("write: bad param index");
        self.mem[q] = v;
    }

    fn run_op2<F>(&mut self, f: F, am: Mode, bm: Mode)
    where
        F: FnOnce(T, T) -> T,
    {
        let a = self.read(self.ip + 1, am);
        let b = self.read(self.ip + 2, bm);
        self.write_param(self.ip + 3, f(a, b));
        self.ip += 4;
    }

    fn run_in1(&mut self, input: T) {
        let p = self.mem[self.ip + 1]
            .try_into()
            .expect("run_in1: bad param index");
        self.mem[p] = input;
        self.ip += 2;
    }

    fn run_in<I>(&mut self, input: &mut I)
    where
        I: Iterator<Item = T>,
    {
        match input.next() {
            Some(i) => self.run_in1(i),
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

    fn run1<I>(&mut self, input: &mut I) -> Option<Output<T>>
    where
        I: Iterator<Item = T>,
    {
        match self.get_op_mode() {
            Op::Add(am, bm) => {
                self.run_op2(|x, y| x + y, am, bm);
                Some(Output::Cont)
            }
            Op::Mul(am, bm) => {
                self.run_op2(|x, y| x * y, am, bm);
                Some(Output::Cont)
            }
            Op::In => {
                self.run_in(input);
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
            Op::Lt(am, bm) => {
                self.run_op2(|x, y| if x < y { T::one() } else { T::zero() }, am, bm);
                Some(Output::Cont)
            }
            Op::Eq(am, bm) => {
                self.run_op2(|x, y| if x == y { T::one() } else { T::zero() }, am, bm);
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
    T: Copy,
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
                Op::Add(am, bm) => {
                    self.run_op2(|x, y| x + y, am, bm);
                }
                Op::Mul(am, bm) => {
                    self.run_op2(|x, y| x * y, am, bm);
                }
                Op::In => {
                    return IO::In(Box::new(|input| {
                        self.run_in1(input);
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
                Op::Lt(am, bm) => {
                    self.run_op2(|x, y| if x < y { T::one() } else { T::zero() }, am, bm);
                }
                Op::Eq(am, bm) => {
                    self.run_op2(|x, y| if x == y { T::one() } else { T::zero() }, am, bm);
                }
                Op::Halt => return IO::Halt,
            }
        }
    }
}
