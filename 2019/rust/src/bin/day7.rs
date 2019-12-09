use std::cmp::max;
use std::collections::VecDeque;
use std::iter::{once, repeat};
use std::mem;

use aoc2019::intcode::*;

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

fn run_chain_param(mach: &IC<i32, Vec<i32>>, params: &[i32], input: i32) -> i32 {
    let mut c = input;
    for p in params {
        let mut is = vec![*p, c].into_iter();
        c = mach.clone().run(&mut is).next().unwrap();
    }
    c
}

struct Trace {
    trace: usize,
    trace_output: VecDeque<i32>,
    init_input: Vec<Vec<i32>>,
    upstream: Vec<usize>,
    state: Vec<Option<IC<i32, Vec<i32>>>>, // None means it is being processed and waiting for input
}

impl Iterator for Trace {
    type Item = i32;
    fn next(&mut self) -> Option<i32> {
        fn go(tr: &mut Trace, t: usize) -> Option<i32> {
            let m = mem::replace(&mut tr.state[t], None).expect("Non-productive loop detected");
            match m.run_to_io() {
                IO::Out(m_new, o) => {
                    tr.state[t] = Some(m_new);
                    Some(o)
                }
                IO::In(f) => match tr.init_input[t].pop() {
                    Some(i) => {
                        tr.state[t] = Some(f(i));
                        go(tr, t)
                    }
                    None => {
                        let u = tr.upstream[t];
                        if u == tr.trace && !tr.trace_output.is_empty() {
                            let o = tr.trace_output.pop_front().unwrap();
                            tr.state[t] = Some(f(o));
                            go(tr, t)
                        } else {
                            match go(tr, tr.upstream[t]) {
                                Some(i) => {
                                    tr.state[t] = Some(f(i));
                                    go(tr, t)
                                }
                                None => panic!("Machine {} starved for input", t),
                            }
                        }
                    }
                },
                IO::Halt => None,
            }
        }
        let o = go(self, self.trace);
        match o {
            Some(out) => self.trace_output.push_back(out),
            None => {}
        }
        o
    }
}

fn run_network_param_trace(
    mach: &IC<i32, Vec<i32>>,
    upstream: &[usize],
    trace: usize,
    params: &[i32],
    input: (i32, usize),
) -> Trace {
    let mut state: Vec<Option<IC<i32, Vec<i32>>>> = Vec::with_capacity(upstream.len());
    for _ in 0..params.len() {
        state.push(Some(mach.clone()));
    }
    let mut init_input: Vec<Vec<i32>> = Vec::with_capacity(params.len());
    for i in 0..params.len() {
        if i == input.1 {
            init_input.push(vec![input.0, params[i]]);
        } else {
            init_input.push(vec![params[i]]);
        }
    }
    Trace {
        trace,
        init_input,
        trace_output: VecDeque::new(),
        upstream: upstream.to_vec(),
        state,
    }
}

fn main() {
    let mem = read_intcode("../data/day7");
    let mach = IC::new(0, mem);

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

    let topo = &[4, 0, 1, 2, 3];
    let mut mx = 0;
    for p in permutations(5) {
        let mut params = Vec::with_capacity(p.len());
        for param in p {
            params.push(5 + param as i32);
        }
        let res = run_network_param_trace(&mach, topo, 4, &params[..], (0, 0));
        mx = max(mx, res.last().unwrap());
    }
    println!("part b: maximum loop-amplified thruster value is {}", mx);
}
