use aoc2019::intcode::*;
use std::collections::{HashSet, VecDeque};

type Pos = (i64, i64);
type Robot = IC<i64, Vec<i64>>;

enum MoveResult {
    Wall,
    Step(Robot, bool),
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
                    1 => (l.0, l.1 + 1),
                    2 => (l.0, l.1 - 1),
                    3 => (l.0 - 1, l.1),
                    4 => (l.0 + 1, l.1),
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

    while let Some((l, s, r)) = fringe.pop_front() {
        let mut f = |d| do_move(&mut fringe, &mut seen, r.clone(), l, s, d);
        f(1).or_else(|| f(2)).or_else(|| f(3)).or_else(|| f(4));
        match do_move(&mut fringe, &mut seen, r, l, s, 4) {
            None => {}
            Some((l, s)) => {
                println!(
                    "part a: the system is at ({},{}), and it takes {} steps to get there",
                    l.0, l.1, s
                );
                break;
            }
        }
    }
}
