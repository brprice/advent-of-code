use aoc2019::intcode::*;
use std::collections::{HashSet, VecDeque};

type Pos = (i64, i64);
type Robot = IC<i64, Vec<i64>>;

enum MoveResult {
    Wall,
    Step(Robot, bool),
}

fn north(l: Pos) -> Pos {
    (l.0, l.1 + 1)
}

fn south(l: Pos) -> Pos {
    (l.0, l.1 - 1)
}

fn east(l: Pos) -> Pos {
    (l.0 + 1, l.1)
}

fn west(l: Pos) -> Pos {
    (l.0 - 1, l.1)
}

fn do_move(
    fr: &mut VecDeque<(Pos, u64, Robot)>,
    se: &mut HashSet<Pos>,
    r: Robot,
    l: Pos,
    st: u64, // steps so far
    d: i64,  // direction command
) -> Option<(Pos, u64)> {
    let result = match r.run_to_io() {
        IO::In(f) => match f(d).run_to_io() {
            IO::Out(s, o) => match o {
                0 => MoveResult::Wall,           // hit wall, didn't find target
                1 => MoveResult::Step(s, false), // step successful, didn't find target
                2 => MoveResult::Step(s, true),  // step successful, did find target
                _ => panic!("robot gave output not in {0,1,2}"),
            },
            _ => panic!("do_move: robot did not give output"),
        },
        _ => panic!("do_move: robot not in input state"),
    };
    match result {
        MoveResult::Wall => return None,
        MoveResult::Step(s, b) => {
            let new_loc = match d {
                1 => north(l),
                2 => south(l),
                3 => west(l),
                4 => east(l),
                _ => panic!("do_move: invalid d"),
            };
            if !se.contains(&new_loc) {
                se.insert(new_loc);
                fr.push_back((new_loc, st + 1, s));
            }
            if b {
                return Some((new_loc, st + 1));
            } else {
                return None;
            }
        }
    }
}

fn parta(fringe: &mut VecDeque<(Pos, u64, Robot)>, seen: &mut HashSet<Pos>) -> (Pos, u64) {
    while let Some((l, s, r)) = fringe.pop_front() {
        let mut f = |d| do_move(fringe, seen, r.clone(), l, s, d);
        f(1).or_else(|| f(2)).or_else(|| f(3)).or_else(|| f(4));
        match do_move(fringe, seen, r, l, s, 4) {
            None => {}
            Some((l, s)) => {
                return (l, s);
            }
        }
    }
    panic!("part a: didn't find oxygen system!")
}

fn main() {
    let robot = read_intcode("../data/day15");
    let robot: Robot = IC::new(0, robot);

    // part a: simple breadth-first search, cloning the entire state
    // starting position: 0,0
    let mut seen = HashSet::new();
    let mut fringe = VecDeque::new();
    // fringe contains location, number of steps to get there, robot state
    fringe.push_back(((0, 0), 0, robot));
    seen.insert((0, 0));
    let (system_loc, steps) = parta(&mut fringe, &mut seen);
    println!(
        "part a: the system is at ({},{}), and it takes {} steps to get there",
        system_loc.0, system_loc.1, steps
    );

    // part b: now continue the search to get a complete map
    while let Some((l, s, r)) = fringe.pop_front() {
        let mut f = |d| do_move(&mut fringe, &mut seen, r.clone(), l, s, d);
        f(1);
        f(2);
        f(3);
        f(4);
    }
    // now seen contains the complete map
    let mut o2_fringe = HashSet::new();
    o2_fringe.insert(system_loc);
    let mut time = 0;
    let mut unoxygenated = seen;
    unoxygenated.remove(&system_loc);
    while !unoxygenated.is_empty() {
        time += 1;
        let mut new_o2_fringe = HashSet::new();
        for o in o2_fringe {
            if unoxygenated.remove(&north(o)) {
                new_o2_fringe.insert(north(o));
            }
            if unoxygenated.remove(&south(o)) {
                new_o2_fringe.insert(south(o));
            }
            if unoxygenated.remove(&west(o)) {
                new_o2_fringe.insert(west(o));
            }
            if unoxygenated.remove(&east(o)) {
                new_o2_fringe.insert(east(o));
            }
        }
        o2_fringe = new_o2_fringe;
    }
    println!("part b: it took {} minutes to reoxygenate", time);
}
