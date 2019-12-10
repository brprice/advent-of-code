use std::cmp::max;
use std::collections::HashSet;
use std::convert::TryInto;
use std::fs;

use num::integer::gcd;

// in increasing distance of first point, bounded by max(w,h)
fn sightlines(w: i32, h: i32) -> Vec<(i32, i32)> {
    let m: i32 = max(w, h);
    let mut res = if m >= 1 {
        vec![
            (1, 0),
            (-1, 0),
            (0, 1),
            (0, -1),
            (1, 1),
            (1, -1),
            (-1, -1),
            (-1, 1),
        ]
    } else {
        Vec::new()
    };
    for x in 1..=m {
        for y in 1..x {
            if gcd(x, y) == 1 {
                res.append(&mut vec![
                    (x, y),
                    (x, -y),
                    (-x, -y),
                    (-x, y),
                    (y, x),
                    (y, -x),
                    (-y, -x),
                    (-y, x),
                ]);
            }
        }
    }
    res
}

// part b: we now want to scan clockwise, so sort sightlines as (offsets)
// (0,-1),(1,-inf..0),(2,-inf..0)...(1,0),
fn sightlines_clockwise(w: i32, h: i32) -> Vec<(i32, i32)> {
    let mut sl = sightlines(w, h);
    let cmp = |&(x, y): &(i32, i32), &(u, v): &(i32, i32)| {
        // NB: positive input direction is right and down, we want to order as "up" before "right"
        // before "down" before "left".
        // atan2(y,x) assumes positive is right and up, and gives clockwise angle from x->+inf, in
        // range (-pi,pi]
        // atan2(-x,y), not atan2(y,x) (written y.atan2(x)) to measure clockwise from "down", with the branch cut "upwards"
        let xf = x as f64;
        let yf = y as f64;
        let uf = u as f64;
        let vf = v as f64;
        ((-xf).atan2(yf)).partial_cmp(&(-uf).atan2(vf)).unwrap()
    };
    sl.sort_unstable_by(cmp);
    sl
}

fn main() {
    let field = fs::read_to_string("../data/day10").unwrap();
    let h: i32 = field.lines().next().unwrap().len().try_into().unwrap();
    let w: i32 = field.lines().count().try_into().unwrap();
    let sightlines = sightlines(w, h);
    let furthest_edge = |&(x, y)| max(max(x, w - 1 - x), max(y, h - 1 - y));
    let asts: HashSet<(i32, i32)> = field
        .lines()
        .enumerate()
        .flat_map(|(y, l)| {
            l.chars()
                .enumerate()
                .filter(|(_, c)| *c == '#')
                .map(move |(x, _)| (x.try_into().unwrap(), y.try_into().unwrap()))
        })
        .collect();
    let get_sight = |asts: &HashSet<_>, ast: &(i32, i32), (sx, sy)| -> Option<(i32, i32)> {
        for k in 1.. {
            let asxk = ast.0 + k * sx;
            let asyk = ast.1 + k * sy;
            if asxk > w || asyk > h || asxk < 0 || asyk < 0 {
                return None;
            };
            if asts.contains(&(asxk, asyk)) {
                return Some((asxk, asyk));
            }
        }
        return None;
    };

    let sees = asts.iter().map(|ast| {
        let m: i32 = furthest_edge(ast).try_into().unwrap();
        let mut see = 0;
        for (sx, sy) in sightlines.iter().take_while(|(sx, sy)| max(sx, sy) <= &m) {
            match get_sight(&asts, ast, (sx, sy)) {
                Some(_) => see += 1,
                None => {}
            }
        }
        (see, ast)
    });
    let (best_sees, best_ast) = sees.max().unwrap();
    let best_ast = &best_ast.clone();
    println!("part a: {:?} sees {} other asteroids", best_ast, best_sees);

    let bets_on = 200;
    let vaporise_lines = sightlines_clockwise(w, h);
    let mut vap = 0;
    let mut remaining = asts;
    for (lx, ly) in vaporise_lines.iter().cycle() {
        match get_sight(&remaining, best_ast, (lx, ly)) {
            Some(a) => {
                vap += 1;
                remaining.remove(&a);
                if vap == bets_on {
                    println!("part b: bet resolution, the {}th asteroid to be vaporised was {:?}, for an answer of {}",bets_on,a,100*a.0+a.1);
                    break;
                }
            }
            None => {}
        }
    }
}
