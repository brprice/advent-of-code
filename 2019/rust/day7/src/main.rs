use std::cmp::max;
use std::fs;
use std::iter::{once, repeat};

#[derive(Clone)]
struct IC {
    ip: usize,
    mem: Vec<i32>,
}
enum Output {
    Cont,
    Out(i32),
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

impl IC {
    fn op_arg_mode(i: i32) -> Op {
        let op = i % 100;
        let mut ams = i / 100;
        let mut modes = std::iter::from_fn(|| {
            let m = ams % 10;
            ams /= 10;
            match m {
                0 => Some(Mode::Pos),
                1 => Some(Mode::Imm),
                m => panic!("Unrecognised mode: {}", m),
            }
        });
        match op {
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

    fn read(&self, a: usize, m: Mode) -> i32 {
        let b = self.mem[a];
        match m {
            Mode::Pos => self.mem[b as usize],
            Mode::Imm => b,
        }
    }

    fn write_param(&mut self, p: usize, v: i32) {
        let q = self.mem[p] as usize;
        self.mem[q] = v;
    }

    fn run_op2<F>(&mut self, f: F, am: Mode, bm: Mode)
    where
        F: FnOnce(i32, i32) -> i32,
    {
        let a = self.read(self.ip + 1, am);
        let b = self.read(self.ip + 2, bm);
        self.write_param(self.ip + 3, f(a, b));
        self.ip += 4;
    }

    fn run_in<I>(&mut self, input: &mut I)
    where
        I: Iterator<Item = i32>,
    {
        let p = self.mem[self.ip + 1] as usize;
        match input.next() {
            Some(i) => self.mem[p] = i,
            None => panic!("Input ran dry"),
        }
        self.ip += 2;
    }

    fn run_out(&mut self, m: Mode) -> i32 {
        let o = self.read(self.ip + 1, m);
        self.ip += 2;
        o
    }

    fn run_ji(&mut self, dir: bool, am: Mode, bm: Mode) {
        let a = self.read(self.ip + 1, am);
        let b = self.read(self.ip + 2, bm);
        if dir == (a != 0) {
            self.ip = b as usize;
        } else {
            self.ip += 3;
        }
    }

    fn run1<I>(&mut self, input: &mut I) -> Option<Output>
    where
        I: Iterator<Item = i32>,
    {
        match IC::op_arg_mode(self.mem[self.ip]) {
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
                self.run_op2(|x, y| (x < y) as i32, am, bm);
                Some(Output::Cont)
            }
            Op::Eq(am, bm) => {
                self.run_op2(|x, y| (x == y) as i32, am, bm);
                Some(Output::Cont)
            }
            Op::Halt => None,
        }
    }

    fn run<'a, I>(&'a mut self, input: &'a mut I) -> impl 'a + Iterator<Item = i32>
    where
        I: Iterator<Item = i32>,
    {
        std::iter::from_fn(move || self.run1(input)).filter_map(|o| match o {
            Output::Cont => None,
            Output::Out(a) => Some(a),
        })
    }
}

fn permutations(n: usize) -> impl Iterator<Item = Vec<usize>> {
    // not going to worry about setting the correct capacity
    fn choose_last(max: usize, v: Vec<usize>) -> impl Iterator<Item = Vec<usize>> {
        let vs = repeat(v);
        vs.zip(0..max).map(|(mut w, l)| {
            for i in 0..w.len() {
                if w[i] >= l {
                    w[i] += 1
                }
            }
            w.push(l);
            w
        })
    }

    fn perm(n: usize) -> Box<dyn Iterator<Item = Vec<usize>>> {
        if n == 0 {
            Box::new(once(Vec::new()))
        } else {
            Box::new(permutations(n - 1).flat_map(move |p| choose_last(n, p)))
        }
    }

    perm(n)
}

fn run_chain_param(mach: &IC, params: &[i32], input: i32) -> i32 {
    let mut c = input;
    for p in params {
        let mut is = vec![*p, c].into_iter();
        c = mach.clone().run(&mut is).next().unwrap();
    }
    c
}

fn main() {
    let data = fs::read_to_string("../../data/day7").unwrap();
    // we trust the input, so just unwrap everything instead of doing error handling
    let mem: Vec<i32> = data.trim().split(',').map(|x| x.parse().unwrap()).collect();
    let mach = IC { ip: 0, mem };

    let mut mx = 0;
    for p in permutations(5) {
        let mut params = Vec::with_capacity(p.len());
        for param in p {
            params.push(param as i32);
        }
        let res = run_chain_param(&mach, &params[..], 0);
        mx = max(mx, res);
    }
    println!("part a: maximum thruster value is {}", mx);
}
