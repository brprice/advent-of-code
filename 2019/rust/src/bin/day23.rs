use num::{zero, Num};
use std::collections::{HashSet, VecDeque};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display};
use std::ops::Sub;

use aoc2019::intcode::*;
use aoc2019::sparse::*;

struct State<T, C: Vector<T>> {
    mach: IC<T, C>,
    buf_in: VecDeque<T>,
    buf_out: Vec<T>,
}

// returns packets sent to machines not in the input vector
// and also whether the network was idle this tick: all queues empty and no outputs happened
// if not, return None, if so, return the set of machines which requested input
// (Then the defininion of "idle network" for part b will be that there have been a sequence of
// idle ticks, during which there was a read by each machine)
fn step<T, C: Vector<T>>(state: &mut Vec<State<T, C>>) -> (Vec<(T, T, T)>, Option<HashSet<usize>>)
where
    T: Clone,
    T: TryInto<usize>,
    T: TryFrom<usize>,
    T: From<u8>,
    T: Num,
    T: Sub,
    T: PartialOrd,
    T: Display,
    T: Debug,
    <T as TryInto<usize>>::Error: Debug,
    <T as TryFrom<usize>>::Error: Debug,
{
    let minus_one = T::from(0u8) - T::from(1u8);
    let mut ret = Vec::new();
    let mut idle = Some(HashSet::new());

    for s in 0..state.len() {
        let i = state[s].buf_in.front();
        let i = match i {
            None => &minus_one,
            Some(x) => {
                idle = None;
                x
            }
        };
        let mut in_iter = vec![i.clone()].into_iter();
        match state[s].mach.run1(&mut in_iter) {
            Some(Output::Cont) => {}
            Some(Output::Out(o)) => {
                idle = None;
                state[s].buf_out.push(o);
            }
            None => {} // halted
        }
        match in_iter.next() {
            None => {
                idle = idle.map(|mut x| {
                    x.insert(s);
                    x
                });
                state[s].buf_in.pop_front();
            } // consumed some input
            Some(_) => {}
        }
        if state[s].buf_out.len() == 3 {
            let y = state[s].buf_out.pop().unwrap();
            let x = state[s].buf_out.pop().unwrap();
            let d = state[s].buf_out.pop().unwrap();
            if d < zero()
                || d > state
                    .len()
                    .try_into()
                    .expect("too many machines, we need a bigger type for step's T")
            {
                ret.push((d, x, y));
            } else {
                let d: usize = d.try_into().unwrap();
                state[d].buf_in.push_back(x);
                state[d].buf_in.push_back(y);
            }
        }
    }
    (ret, idle)
}

fn main() {
    // part a: we can get away with i64, rather than bignums
    let mem: Sparse<usize, i64> = read_intcode_sparse("../data/day23");
    let mach = IC::new(0, mem.clone());

    let num_nics = 50;
    let mut state: Vec<State<_, _>> = Vec::with_capacity(num_nics);
    for addr in 0..num_nics {
        let m = mach.clone();
        let mut buf_in = VecDeque::with_capacity(1);
        buf_in.push_back(addr.try_into().unwrap());
        state.push(State {
            mach: m,
            buf_in,
            buf_out: Vec::new(),
        });
    }

    // for part b, initialised with garbage, will be overwritten in part a
    let mut nat = (0, 0);

    // part a: it turns out that the first packet sent to an address not in the network
    // is sent to 255, as the puzzle asks.
    loop {
        let (ps, _) = step(&mut state);
        if !ps.is_empty() {
            println!("part a: first out-of-network packets:");
            for (d, x, y) in ps {
                println!("dst: {} X:{} Y:{}", d, x, y);
                // for part b
                if d == 255 {
                    nat = (x, y);
                }
            }
            break;
        }
    }

    // part b:
    let all_idle = |is: &HashSet<_>| (0..num_nics).all(|i| is.contains(&i));
    let mut idles = HashSet::new();
    let mut last_y = None;
    loop {
        let (ps, idle) = step(&mut state);
        for (d, x, y) in ps {
            if d == 255 {
                nat = (x, y);
            }
        }
        match idle {
            None => idles.clear(),
            Some(is) => idles = &idles | &is,
        }
        if all_idle(&idles) {
            state[0].buf_in.push_back(nat.0);
            state[0].buf_in.push_back(nat.1);
            let new_y = Some(nat.1);
            if last_y == new_y {
                println!("part b: first Y delivired by NAT twice in a row: {}", nat.1);
                break;
            }
            last_y = new_y;
        }
    }
}
