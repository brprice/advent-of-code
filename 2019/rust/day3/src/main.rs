use std::fmt;
use std::fs;

#[derive(Debug)]
enum Dir {
    U,
    D,
    L,
    R,
}
#[derive(Debug)]
struct Point<T> {
    x: T,
    y: T,
}

impl<T: fmt::Display> fmt::Display for Point<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

#[derive(Debug)]
struct Seg {
    start: Point<i32>,
    end: Point<i32>,
}

impl Seg {
    fn new((x, y): (i32, i32), (s, t): (i32, i32)) -> Seg {
        Seg {
            start: Point { x, y },
            end: Point { x: s, y: t },
        }
    }
    fn intersection(&self, l: &Seg) -> Option<Point<f32>> {
        let sdx = self.end.x - self.start.x;
        let sdy = self.end.y - self.start.y;
        let ldx = l.end.x - l.start.x;
        let ldy = l.end.y - l.start.y;
        let d = sdx * ldy - sdy * ldx;
        if d != 0 {
            let tn = ((l.start.x - self.start.x) * ldy - (l.start.y - self.start.y) * ldx) as f32;
            let un = (-sdx * (l.start.y - self.start.y) + sdy * (l.start.x - self.start.x)) as f32;
            let d = d as f32;
            // non-parallel
            if 0.0 <= tn / d && tn / d <= 1.0 && 0.0 <= un / d && un / d <= 1.0 {
                // intersects inside segments
                let sdx = sdx as f32;
                let sdy = sdy as f32;
                let x = self.start.x as f32;
                let y = self.start.y as f32;
                Some(Point {
                    x: x + tn / d * sdx,
                    y: y + tn / d * sdy,
                })
            } else {
                None
            } // outside segment
        } else {
            // parallel
            None // This is a cheat!
        }
    }
}

fn l1(p: &Point<f32>) -> f32 {
    f32::abs(p.x) + f32::abs(p.y)
}

fn main() {
    let data = fs::read_to_string("../../data/day3").unwrap();
    let parse = |s: &str| -> (Dir, i32) {
        let (d, l) = s.split_at(1);
        match d {
            "U" => (Dir::U, l.parse().unwrap()),
            "D" => (Dir::D, l.parse().unwrap()),
            "L" => (Dir::L, l.parse().unwrap()),
            "R" => (Dir::R, l.parse().unwrap()),
            dir => panic!("unknown direction: {}", dir),
        }
    };
    let mut wires = data.lines();
    let segs = |(x, y): &mut (_, _), (d, l)| {
        let xold = *x;
        let yold = *y;
        match d {
            Dir::U => {
                *y += l;
                Some(Seg::new((xold, yold), (*x, *y)))
            }
            Dir::D => {
                *y -= l;
                Some(Seg::new((xold, yold), (*x, *y)))
            }
            Dir::L => {
                *x -= l;
                Some(Seg::new((xold, yold), (*x, *y)))
            }
            Dir::R => {
                *x += l;
                Some(Seg::new((xold, yold), (*x, *y)))
            }
        }
    };
    let wire1 = wires
        .next()
        .unwrap()
        .split(',')
        .map(parse)
        .scan((0, 0), segs);
    let wire2: Vec<Seg> = wires
        .next()
        .unwrap()
        .split(',')
        .map(parse)
        .scan((0, 0), segs)
        .collect();

    let mut min = Point { x: 0.0, y: 0.0 };
    let mut minl1 = 0.0;

    for s in wire1 {
        for t in &wire2 {
            match s.intersection(t) {
                Some(x) => {
                    let xl1 = l1(&x);
                    if minl1 == 0.0 || minl1 > xl1 {
                        min = x;
                        minl1 = xl1
                    }
                }
                None => {}
            }
        }
    }
    println!(
        "Intersection closest to origin: {}, has l1 norm of {}",
        min, minl1
    );
}
