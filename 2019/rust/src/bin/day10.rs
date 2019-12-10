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

    let sees = asts.iter().map(|ast| {
        let m: i32 = furthest_edge(ast).try_into().unwrap();
        let mut see = 0;
        for (sx, sy) in sightlines.iter().take_while(|(sx, sy)| max(sx, sy) <= &m) {
            for k in 1.. {
                let asxk = ast.0 + k * sx;
                let asyk = ast.1 + k * sy;
                if asxk > w || asyk > h || asxk < 0 || asyk < 0 {
                    break;
                };
                if asts.contains(&(asxk, asyk)) {
                    see += 1;
                    break;
                }
            }
        }
        (see, ast)
    });
    let (best_sees, best_ast) = sees.max().unwrap();
    println!("part a: {:?} sees {} other asteroids", best_ast, best_sees);
}
